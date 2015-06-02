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

-module(ui_root).
-include_lib("rdp_proto/include/rdpp.hrl").
-include("ui.hrl").
-include_lib("cairerl/include/cairerl.hrl").

-export([handle/2]).

-record(state, {bgcolor = {0,0,0}, focus=[]}).

rect_order({W, H}, {R,G,B}, Fmt) ->
    Image0 = #cairo_image{width = round(W), height = round(H),
        format = Fmt, data = <<>>},
    {ok, _, Image1} = cairerl_nif:draw(Image0, [], [
        #cairo_set_source_rgba{r = float(R), g = float(G), b = float(B)},
        #cairo_rectangle{width=W,height=H},
        #cairo_fill{}
        ]),
    #image{image = Image1}.

handle(init, Wd = #widget{size = Sz, format = Fmt}) ->
    State = #state{},
    {ok, Wd#widget{state = State, orders = [
        rect_order(Sz, State#state.bgcolor, Fmt)
    ]}, []};

handle(focus_next, Wd = #widget{state = S}) ->
    #state{focus = Focus} = S,
    Focusable = ui:select(Wd, [{tag, focusable}]),
    ToFocus = case lists:dropwhile(fun(K) -> not lists:member(K#widget.id, Focus) end, Focusable) of
        [_InFocus, Next | _Rest] -> Next;
        _ -> [Next | _] = Focusable, Next
    end,
    NewIds = [ToFocus#widget.id],
    FocusEvts = [{ [{id, Id}], focus } || Id <- NewIds],
    {ok, Wd, FocusEvts};

handle({children_updated, _OldKids}, Wd = #widget{children = _NewKids, state = S = #state{focus = OldFocus}}) ->
    NewFocus = ui:select(Wd, [{tag, focus}]),
    case [K#widget.id || K <- NewFocus] of
        OldFocus ->
            {ok, Wd, []};
        AllNewIds ->
            NewIds = AllNewIds -- OldFocus,
            UnfocusEvts = [{ [{id, Id}], blur } || Id <- OldFocus],
            S2 = S#state{focus = NewIds},
            {ok, Wd#widget{state = S2}, UnfocusEvts}
    end;

handle({add_child, K}, Wd = #widget{children = Kids, size = Sz, format = F}) ->
    case Kids of
        [] -> ok;
        _ -> lager:warning("root widget replacing child")
    end,
    K2 = case K#widget.id of
        undefined -> K#widget{id = make_ref(), dest = {0.0,0.0}, format = F};
        _ -> K#widget{dest = {0.0,0.0}, format = F}
    end,
    {ok, Wd#widget{children = [K2]}, [{ [{id, K2#widget.id}], {resize, Sz} }]};

handle({remove_child, Sel}, Wd = #widget{children = Kids}) ->
    {_Deleted, Kept} = lists:partition(fun(K) ->
        ui:selector_matches(K, Sel)
    end, Kids),
    {ok, Wd#widget{children = Kept}, []};

handle({resize, NewSize}, Wd = #widget{state = S = #state{bgcolor = Bg}, children = Kids, format = Fmt}) ->
    {ok, Wd#widget{state = S, size = NewSize, orders = [
        rect_order(NewSize, Bg, Fmt)
    ]}, [{ [{id, Kid#widget.id}], {resize, NewSize} } || Kid <- Kids]};

handle({set_bgcolor, BgColor}, Wd = #widget{size = Size, state = S = #state{}, format = Fmt}) ->
    S2 = S#state{bgcolor = BgColor},
    {ok, Wd#widget{state = S2, orders = [
        rect_order(Size, BgColor, Fmt)
    ]}, []};

handle(Event, Wd) ->
    ui:default_handler(Event, Wd).
