%%
%% rdpproxy
%% remote desktop proxy
%%
%% Copyright 2012-2015 Alex Wilson <alex@uq.edu.au>
%% The University of Queensland
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-module(ui).
-include_lib("rdp_proto/include/rdpp.hrl").
-include("ui.hrl").
-include_lib("cairerl/include/cairerl.hrl").

-export([new/1, new/2, handle_events/2, select/2, print/1]).
-export([default_handler/2, selector_matches/2]).
-export([divide_bitmap/2, divide_bitmap/1, orders_to_updates/2]).

-spec new(Size :: size()) -> {widget(), [order()]}.
new(Size) -> new(Size, rgb24).

-spec new(Size :: size(), Format :: cairerl:pixel_format()) -> {widget(), [order()]}.
new(Size, Format) ->
    Root = #widget{id = root, mod = ui_root, size = Size, dest = {0.0, 0.0}, format = Format},
    handle_events(Root, [{ [{id, root}], init }]).

-type selector() :: {id, term()} |
                    {contains, point()} |
                    {mod, atom()} |
                    {tag, atom()}.

-spec rect_contains(Dest :: point(), Size :: size(), Point :: point()) -> boolean().
rect_contains({X0, Y0}, {W, H}, {X, Y}) ->
    (X >= X0) andalso (Y >= Y0) andalso
    (X =< X0 + W) andalso (Y =< Y0 + H).

-spec selector_matches(widget(), selector()) -> boolean().
selector_matches(W, {id, Id}) -> W#widget.id =:= Id;
selector_matches(W, {mod, H}) -> W#widget.mod =:= H;
selector_matches(W, {contains, P}) -> rect_contains({0,0}, W#widget.size, P);
selector_matches(W, {tag, T}) -> lists:member(T, W#widget.tags).

-spec collect_orders(widget()) -> [order()].
collect_orders(#widget{orders = O, children = Ks}) ->
    O ++ lists:flatmap(fun(K) -> offset_orders(K#widget.dest, collect_orders(K)) end, Ks).

-spec offset_orders(point(), [order()]) -> [order()].
offset_orders(_O, []) -> [];
offset_orders(O = {X0, Y0}, [R = #rect{dest = {X, Y}} | Rest]) ->
    [R#rect{dest = {X0 + X, Y0 + Y}} | offset_orders(O, Rest)];
offset_orders(O = {X0, Y0}, [I = #image{dest = {X, Y}} | Rest]) ->
    [I#image{dest = {X0 + X, Y0 + Y}} | offset_orders(O, Rest)];
offset_orders(O, [N = #null_order{} | Rest]) ->
    [N | offset_orders(O, Rest)].

-spec offset_selectors(point(), [selector()]) -> [selector()].
offset_selectors(_O, []) -> [];
offset_selectors(O = {X0, Y0}, [{contains, {X, Y}} | Rest]) ->
    [{contains, {X - X0, Y - Y0}} | offset_selectors(O, Rest)];
offset_selectors(O, [Other | Rest]) ->
    [Other | offset_selectors(O, Rest)].

offset_event({X0, Y0}, Evt = #ts_inpevt_mouse{point = {X, Y}}) ->
    Evt#ts_inpevt_mouse{point = {X - X0, Y - Y0}};
offset_event(_O, Evt) -> Evt.

print(W) ->
    print(W, []).
print(#widget{dest = D, size = S, mod = M, id = Id, children = Kids}, Indent) ->
    io:format("~s#widget{id = ~p, dest = ~p, size = ~p, mod = ~p}\n", [Indent, Id, D, S, M]),
    lists:foreach(fun(Kid) ->
        print(Kid, [16#20 | Indent])
    end, Kids).

select(W = #widget{children = []}, [{contains, Pt}]) ->
    case selector_matches(W, {contains, Pt}) of
        true ->
            [W];
        false ->
            []
    end;
select(W = #widget{}, [Selector = {Type, _}]) ->
    case selector_matches(W, Selector) of
        true when Type =/= contains ->
            [W];
        _ ->
            lists:flatmap(fun(Kid) ->
                select(Kid, offset_selectors(W#widget.dest, [Selector]))
            end, W#widget.children)
    end;
select(W = #widget{}, [Selector = {Type, _} | SelRest]) ->
    case selector_matches(W, Selector) of
        true when Type =/= contains ->
            lists:flatmap(fun(Kid) ->
                select(Kid, offset_selectors(W#widget.dest, SelRest))
            end, W#widget.children);
        _ ->
            lists:flatmap(fun(Kid) ->
                select(Kid, offset_selectors(W#widget.dest, [Selector | SelRest]))
            end, W#widget.children)
    end.

handle_events(W = #widget{id = root}, []) -> {W, [], []};
handle_events(W = #widget{id = root}, [{ui, Event} | Rest]) ->
    {W2, RestOrders, RestUiEvts} = handle_events(W, Rest),
    {W2, RestOrders, [Event | RestUiEvts]};
handle_events(W = #widget{id = root}, [{Selector, Event} | Rest]) ->
    {W2, Orders, MoreEvts} = handle(W, Selector, Event),
    {W3, RestOrders, RestUiEvts} = handle_events(W2, Rest ++ MoreEvts),
    {W3, lists:flatten([Orders, RestOrders]), RestUiEvts}.

handle(W = #widget{mod = M, children = [], orders = OldOrders}, [{contains, Pt}], Event) ->
    case selector_matches(W, {contains, Pt}) of
        true ->
            case M:handle(Event, W) of
                {ok, W2 = #widget{orders = OldOrders, children = []}, MoreEvts} ->
                    {W2, [], MoreEvts};
                {ok, W2 = #widget{orders = Orders}, MoreEvts} ->
                    {W2, Orders, MoreEvts}
            end;
        false ->
            {W, [], []}
    end;
handle(W = #widget{mod = M, orders = OldOrders, children = Kids}, [Selector = {Type, _}], Event) ->
    Action = case selector_matches(W, Selector) of
        true when Type =/= contains -> deliver;
        true when Type =:= contains ->
            case lists:any(fun(K = #widget{dest = KD}) ->
                    [OffsetSel] = offset_selectors(KD, [Selector]),
                    selector_matches(K, OffsetSel) end, Kids) of
                false -> deliver;
                true -> recurse
            end;
        _ -> recurse
    end,
    case Action of
        deliver ->
            case M:handle(Event, W) of
                {ok, W2 = #widget{orders = OldOrders, children = Kids}, MoreEvts} ->
                    {W2, [], MoreEvts};
                {ok, W2 = #widget{}, MoreEvts} ->
                    {W2, collect_orders(W2), MoreEvts}
            end;
        recurse ->
            NewKOs = lists:map(fun(Kid) ->
                handle(Kid, offset_selectors(Kid#widget.dest, [Selector]),
                    offset_event(Kid#widget.dest, Event))
            end, Kids),
            handle_recurse(W, NewKOs)
    end;
handle(W = #widget{children = K}, [Selector = {Type, _} | SelRest], Event) ->
    NewKOs = case selector_matches(W, Selector) of
        true when Type =/= contains ->
            lists:map(fun(Kid = #widget{dest = KD}) ->
                handle(Kid, offset_selectors(KD, SelRest), offset_event(KD, Event))
            end, K);
        _ ->
            lists:map(fun(Kid = #widget{dest = KD}) ->
                handle(Kid, offset_selectors(KD, [Selector | SelRest]), offset_event(KD, Event))
            end, K)
    end,
    handle_recurse(W, NewKOs).

handle_recurse(W = #widget{mod = M, orders = OldOrders, children = Kids}, NewKOs) ->
    NewKids = [K || {K, _O, _Es} <- NewKOs],
    Evts = lists:flatmap(fun({_K, _O, Es}) -> Es end, NewKOs),
    W2 = W#widget{children = NewKids},
    case lists:any(fun({_, [], _}) -> false; ({_, _, _}) -> true end, NewKOs) of
        false ->
            {W2, [], Evts};
        true ->
            case M:handle({children_updated, Kids}, W2) of
                {ok, W3 = #widget{orders = OldOrders, children = NewKids}, MoreEvts} ->
                    Orders = lists:flatmap(fun({#widget{dest = KD}, O, _Es}) ->
                        offset_orders(KD, O)
                    end, NewKOs),
                    case Orders of
                        [#null_order{} | _] ->
                            {W3, collect_orders(W3), Evts ++ MoreEvts};
                        _ ->
                            {W3, Orders, Evts ++ MoreEvts}
                    end;
                {ok, W3 = #widget{}, MoreEvts} ->
                    {W3, collect_orders(W3), Evts ++ MoreEvts}
            end
    end.

% default handler for widget implementations to use

default_handler({children_updated, _OldKids}, Wd = #widget{}) ->
    {ok, Wd, []};
default_handler({add_child, K}, Wd = #widget{children = Kids, format = Fmt}) ->
    K2 = case K#widget.id of
        undefined -> K#widget{id = make_ref(), format = Fmt};
        _ -> K#widget{format = Fmt}
    end,
    {ok, Wd#widget{children = Kids ++ [K2]}, []};
default_handler({add_child, {before, Sel}, K}, Wd = #widget{children = Kids, format = Fmt}) ->
    K2 = case K#widget.id of
        undefined -> K#widget{id = make_ref(), format = Fmt};
        _ -> K#widget{format = Fmt}
    end,
    {BeforeNew, AfterNew} = lists:splitwith(fun(Kid) -> not selector_matches(Kid, Sel) end, Kids),
    {ok, Wd#widget{children = BeforeNew ++ [K2 | AfterNew]}, []};
default_handler({remove_child, Sel}, Wd = #widget{children = Kids}) ->
    {_Deleted, Kept} = lists:partition(fun(K) ->
        selector_matches(K, Sel)
    end, Kids),
    {ok, Wd#widget{children = Kept}, []};
default_handler(#ts_inpevt_mouse{action=move}, Wd) ->
    MouseWasIn = select(Wd, [{tag, mouse_in}]),
    Evts = [{ [{id, Id}], mouse_out } || W = #widget{id = Id} <- MouseWasIn, W =/= Wd],
    {ok, Wd, Evts};
default_handler(#ts_inpevt_mouse{}, Wd) ->
    {ok, Wd, []};
default_handler(Event, Wd = #widget{id = Id, mod = M}) ->
    lager:debug("<widget ~p (~p)> ignored event ~p", [Id, M, Event]),
    {ok, Wd, []}.

% ts utils

-type bitmap_slice() :: {X :: integer(), Y :: integer(), #cairo_image{}}.
-spec slice_bitmap(#cairo_image{}, slicerect()) -> bitmap_slice().
slice_bitmap(I = #cairo_image{format = Fmt}, {FromX, FromY, Width, Height}) ->
    Image0 = #cairo_image{width = Width, height = Height, data = <<>>, format = Fmt},
    {ok, _, Image1} = cairerl_nif:draw(Image0, [], [
        #cairo_pattern_create_for_surface{tag=img, image=I},
        #cairo_pattern_translate{tag=img, x=float(FromX), y=float(FromY)},
        #cairo_set_source{tag=img},
        #cairo_rectangle{width = float(Width), height = float(Height)},
        #cairo_fill{}
    ]),
    {FromX, FromY, Image1}.

-spec divide_bitmap(#cairo_image{}) -> [#ts_bitmap{}].
divide_bitmap(I) ->
    divide_bitmap(I, {0, 0}).
-spec divide_bitmap(#cairo_image{}, {X :: integer(), Y :: integer()}) -> [#ts_bitmap{}].
divide_bitmap(I = #cairo_image{width = W, height = H}, Offset = {_X, _Y}) ->
    Slices = compute_slices(W, H),
    [to_ts_bitmap(slice_bitmap(I, S), Offset) || S <- Slices].

-spec to_ts_bitmap(bitmap_slice(), {integer(), integer()}) -> #ts_bitmap{}.
to_ts_bitmap({X, Y,
        #cairo_image{data = D, width = W, height = H, format = Fmt}},
        {X0, Y0}) ->
    Bpp = case Fmt of
        rgb24 -> 24;
        rgb16_565 -> 16;
        _ -> error({bad_format, Fmt})
    end,
    {ok, Compr} = rle_nif:compress(D, W, H, Bpp),
    CompInfo = #ts_bitmap_comp_info{
        flags = [compressed]},
        % full_size = byte_size(D),
        % scan_width = W},
    true = (byte_size(Compr) < 1 bsl 16),
    Dest = {lists:max([0, X0+X]), lists:max([0, Y0+Y])},
    #ts_bitmap{dest=Dest, size={W,H}, bpp=Bpp, data = Compr,
        comp_info = CompInfo}.

%% Try to divide bitmaps into slices 96x96 pixels.
-define(BITMAP_SLICE_SIZE, 96).

-type slicerect() :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.

%% @doc Slices a WxH bitmap into slice rects with max size BITMAP_SLICE_SIZE.
-spec compute_slices(integer(), integer()) -> [slicerect()].
compute_slices(W, H) ->
    compute_x_slices(W, H, 0).

%% Iterates horizontally across the image, calling compute_y_slices() to compute
%% a vertical strip of slices at each X coordinate.
-spec compute_x_slices(integer(), integer(), integer()) -> [slicerect()].
compute_x_slices(W, _H, X) when X >= W -> [];
compute_x_slices(W, H, X) ->
    compute_y_slices(W, H, X, 0) ++
        compute_x_slices(W, H, X + ?BITMAP_SLICE_SIZE).

%% Computes a vertical strip of slices at a fixed X coordinate.
-spec compute_y_slices(integer(), integer(), integer(), integer()) -> [slicerect()].
compute_y_slices(_W, H, _X, Y) when Y >= H -> [];
compute_y_slices(W, H, X, Y) ->
    X1 = lists:min([W, X + ?BITMAP_SLICE_SIZE]),
    Y1 = lists:min([H, Y + ?BITMAP_SLICE_SIZE]),
    [{X, Y, X1 - X, Y1 - Y} | compute_y_slices(W, H, X, Y1)].

keep_last_root(_SoFar, [O = #image{root = true} | Rest]) ->
    keep_last_root([O], Rest);
keep_last_root(SoFar, [O | Rest]) ->
    keep_last_root(SoFar ++ [O], Rest);
keep_last_root(SoFar, []) ->
    SoFar.

orders_to_updates(Orders, Fmt) ->
    T1 = os:timestamp(),
    FromLastRoot = keep_last_root([], Orders),
    PrimUpdates = orders_to_prim_updates(FromLastRoot, Fmt),
    Culled = cull_prims(lists:reverse(PrimUpdates)),
    Updates = pack_prims(Culled),
    T2 = os:timestamp(),
    Delta = timer:now_diff(T2,T1)/1000,
    if (Delta > 5) ->
        lager:debug("processing orders took ~p ms", [Delta]);
    true -> ok end,
    Updates.

-spec binsub(bitstring(), bitstring()) -> bitstring().
binsub(<<>>, <<>>) -> <<>>;
binsub(B1, B2) ->
    Sz = bit_size(B1),
    Sz = bit_size(B2),
    <<N:Sz>> = B1,
    <<M:Sz>> = B2,
    X = N band (bnot M),
    <<X:Sz>>.

-type ts_order() :: #ts_order_opaquerect{} | #ts_bitmap{}.
-type rect() :: {X :: integer(), Y :: integer(), X2 :: integer(), Y2 :: integer()}.
-spec order_geom(ts_order()) -> rect().
-define(clip(X), if ((X) < 0) -> 0; true -> (X) end).
-define(min(X,Y), if ((X) < (Y)) -> (X); true -> (Y) end).
-define(max(X,Y), if ((X) > (Y)) -> (X); true -> (Y) end).
-define(bound(X,X0,X1), if ((X) < (X0)) -> X0; ((X) > (X1)) -> X1; true -> X end).
order_geom(#ts_order_opaquerect{dest={X,Y},size={W,H}}) ->
    {X,Y,X+W,Y+H};
order_geom(#ts_bitmap{dest={X,Y},size={W,H}}) ->
    {X,Y,X+W,Y+H}.

rect_overlap({AX, AY, AX2, AY2}, {BX, BY, BX2, BY2}) ->
    not (
        (AX > BX2) orelse
        (AY > BY2) orelse
        (AX2 < BX) orelse
        (AY2 < BY)
        ).

-spec make_bitmap_mask(Geom :: rect(), ParentGeom :: rect()) -> bitstring().
make_bitmap_mask({X,Y,X2,Y2}, {PX,PY,PX2,PY2}) ->
    Scan = PX2 - PX,
    OnesStart = ?bound(X, PX, PX2),
    PreZeroLen = OnesStart - PX,
    OnesFinish = ?bound(X2, OnesStart, PX2),
    OnesLen = OnesFinish - OnesStart,
    PostZeroLen = PX2 - OnesFinish,
    Scan = PreZeroLen + OnesLen + PostZeroLen,
    lists:foldl(fun(CY, Acc) ->
        if
            (CY >= Y) andalso (CY =< Y2) ->
                <<0:PreZeroLen, (1 bsl OnesLen - 1):OnesLen, 0:PostZeroLen, Acc/bitstring>>;
            true ->
                <<0:Scan, Acc/bitstring>>
        end
    end, <<>>, lists:reverse(lists:seq(PY, PY2))).

-record(mask, {bitmap, geom, order}).

cull_prims(Prims) ->
    Masks0 = [#mask{geom = order_geom(P), order = P}
        || P <- Prims],
    Masks1 = cull_identical(Masks0),
    Masks2 = [M#mask{bitmap = make_bitmap_mask(G, G)}
        || M = #mask{geom = G} <- Masks1],
    Masks3 = cull_masks(Masks2),
    [M#mask.order || M <- Masks3].

cull_identical(List) -> cull_identical(List, gb_trees:empty()).
cull_identical([], _) -> [];
cull_identical([M = #mask{geom = G} | Rest], Seen) ->
    case gb_trees:is_defined(G, Seen) of
        true -> cull_identical(Rest, Seen);
        false -> [M | cull_identical(Rest, gb_trees:insert(G, true, Seen))]
    end.

cull_masks([]) -> [];
cull_masks([M = #mask{bitmap = B, geom = G} | Rest]) ->
    N = bit_size(B),
    case B of
        <<0:N>> ->
            cull_masks(Rest);
        _ ->
            SubRest = lists:map(fun(M2 = #mask{bitmap = B2, geom = G2}) ->
                case rect_overlap(G, G2) of
                    true ->
                        BM = make_bitmap_mask(G, G2),
                        B22 = binsub(B2, BM),
                        M2#mask{bitmap = B22};
                    false ->
                        M2
                end
            end, Rest),
            [M | cull_masks(SubRest)]
    end.

orders_to_prim_updates([], _Fmt) -> [];
orders_to_prim_updates([Rect = #rect{} | Rest], Fmt) ->
    {Bpp,RBits,GBits,BBits} = case Fmt of
        rgb24 -> {24,8,8,8};
        rgb16_565 -> {16,5,6,5};
        _ -> error({bad_format, Fmt})
    end,
    #rect{dest={X,Y}, size={W,H}, color={R,G,B}} = Rect,
    [#ts_order_opaquerect{dest={round(X),round(Y)},
                size={round(W),round(H)},
                bpp=Bpp,
                color={
                    trunc(R*(1 bsl RBits)),
                    trunc(G*(1 bsl GBits)),
                    trunc(B*(1 bsl BBits))}} |
        orders_to_prim_updates(Rest, Fmt)];
orders_to_prim_updates([#image{dest = {X,Y}, image = Im} | Rest], Fmt) ->
    #cairo_image{format = Fmt} = Im,
    Bitmaps = divide_bitmap(Im, {round(X), round(Y)}),
    Bitmaps ++ orders_to_prim_updates(Rest, Fmt);
orders_to_prim_updates([#null_order{} | Rest], Fmt) ->
    orders_to_prim_updates(Rest, Fmt).

pack_prims([]) -> [];
pack_prims(L = [#ts_order_opaquerect{} | _]) ->
    {Rects, Rest} = lists:splitwith(fun
        (#ts_order_opaquerect{}) -> true;
        (_) -> false
    end, L),
    [#ts_update_orders{orders = Rects} | pack_prims(Rest)];
pack_prims(L = [#ts_bitmap{} | _]) ->
    {Bitmaps, Rest} = lists:splitwith(fun
        (#ts_bitmap{}) -> true;
        (_) -> false
    end, L),
    bitmaps_to_orders(Bitmaps) ++ pack_prims(Rest).

bitmaps_to_orders(Bms) ->
    lists:reverse(bitmaps_to_orders(0, [], Bms)).

bitmaps_to_orders(_, [], []) -> [];
bitmaps_to_orders(_, R, []) ->
    [#ts_update_bitmaps{bitmaps = lists:reverse(R)}];
bitmaps_to_orders(Size, R, [Next | Rest]) ->
    #ts_bitmap{data = D} = Next,
    NewSize = Size + byte_size(D),
    if
        (NewSize > 16000) or (length(R) > 16) ->
            [#ts_update_bitmaps{bitmaps = lists:reverse(R)} |
                bitmaps_to_orders(0, [Next], Rest)];
        true ->
            bitmaps_to_orders(NewSize, [Next | R], Rest)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

y_slices_simple_test() ->
    Res = compute_y_slices(96, 288, 0, 0),
    ?assertMatch([
        {0, 0, 96, 96},
        {0, 96, 96, 96},
        {0, 192, 96, 96}
        ], Res).
y_slices_clip_y_test() ->
    Res = compute_y_slices(96, 300, 0, 0),
    ?assertMatch([
        {0, 0, 96, 96},
        {0, 96, 96, 96},
        {0, 192, 96, 96},
        {0, 288, 96, 12}
        ], Res).
y_slices_clip_xy_test() ->
    Res = compute_y_slices(90, 300, 0, 0),
    ?assertMatch([
        {0, 0, 90, 96},
        {0, 96, 90, 96},
        {0, 192, 90, 96},
        {0, 288, 90, 12}
        ], Res).

x_slices_simple_test() ->
    Res = compute_x_slices(96, 288, 0),
    ?assertMatch([
        {0, 0, 96, 96},
        {0, 96, 96, 96},
        {0, 192, 96, 96}
        ], Res).
x_slices_two_col_test() ->
    Res = compute_x_slices(192, 288, 0),
    ?assertMatch([
        {0, 0, 96, 96},
        {0, 96, 96, 96},
        {0, 192, 96, 96},
        {96, 0, 96, 96},
        {96, 96, 96, 96},
        {96, 192, 96, 96}
        ], Res).
x_slices_two_col_clip_test() ->
    Res = compute_x_slices(180, 300, 0),
    ?assertMatch([
        {0, 0, 96, 96},
        {0, 96, 96, 96},
        {0, 192, 96, 96},
        {0, 288, 96, 12},
        {96, 0, 84, 96},
        {96, 96, 84, 96},
        {96, 192, 84, 96},
        {96, 288, 84, 12}
        ], Res).


-endif.
