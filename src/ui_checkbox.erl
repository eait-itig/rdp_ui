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

-module(ui_checkbox).
-include_lib("rdp_proto/include/rdpp.hrl").
-include("ui.hrl").
-include_lib("cairerl/include/cairerl.hrl").

-export([handle/2, get_text/1, get_checked/1]).

color_set_order({R, G, B}) ->
    #cairo_set_source_rgba{r = float(R), g = float(G), b = float(B)}.

-record(state, {align, text, checked=false, bgcolor={0,0,0}, fgcolor={1,1,1}}).

get_text(#widget{state = #state{text = Text}}) ->
    Text.

get_checked(#widget{state = #state{checked = Checked}}) ->
    Checked.

handle({init, Align, Text}, Wd = #widget{}) ->
    handle(redraw, Wd#widget{tags = [focusable], state = #state{align = Align, text = Text}});

handle({set_fgcolor, Color}, Wd = #widget{state = S}) ->
    S2 = S#state{fgcolor = Color},
    handle(redraw, Wd#widget{state = S2});

handle({set_bgcolor, Color}, Wd = #widget{state = S}) ->
    S2 = S#state{bgcolor = Color},
    handle(redraw, Wd#widget{state = S2});

handle({set_text, Text}, Wd = #widget{state = S}) ->
    S2 = S#state{text = Text},
    handle(redraw, Wd#widget{state = S2});

handle({set_checked, Checked}, Wd = #widget{state = S}) ->
    S2 = S#state{checked = Checked},
    handle(redraw, Wd#widget{state = S2});

handle(focus, Wd = #widget{tags = T, state = S}) ->
    S2 = S#state{},
    case lists:member(focus, T) of
        true ->
            {ok, Wd, []};
        false ->
            handle(redraw, Wd#widget{state = S2, tags = [focus | T]})
    end;

handle(#ts_inpevt_mouse{action = move}, Wd = #widget{tags = T}) ->
    case lists:member(mouse_in, T) of
        true -> {ok, Wd, []};
        false -> handle(redraw, Wd#widget{tags = [mouse_in | T]})
    end;

handle(mouse_out, Wd = #widget{tags = T}) ->
    case lists:member(mouse_in, T) of
        false -> {ok, Wd, []};
        _ -> handle(redraw, Wd#widget{tags = T -- [mouse_in]})
    end;

handle(blur, Wd = #widget{tags = T}) ->
    case lists:member(focus, T) of
        false -> {ok, Wd, []};
        _ -> handle(redraw, Wd#widget{tags = T -- [focus]})
    end;

handle(#ts_inpevt_key{code = enter, action = down}, Wd = #widget{id = Id}) ->
    {ok, Wd, [{ui, {submitted, Id}}]};

handle(#ts_inpevt_key{code = space, action = down}, Wd = #widget{state = S}) ->
    S2 = case S of
        #state{checked = true} -> S#state{checked = false};
        #state{checked = false} -> S#state{checked = true}
    end,
    handle(redraw, Wd#widget{state = S2});

handle(#ts_inpevt_mouse{action = down, buttons = [N]}, Wd = #widget{state = S})
                                            when (N == 1) or (N == 2) ->
    S2 = case S of
        #state{checked = true} -> S#state{checked = false};
        #state{checked = false} -> S#state{checked = true}
    end,
    handle(redraw, Wd#widget{state = S2});

handle({resize, {W,H}}, Wd = #widget{state = S}) ->
    handle(redraw, Wd#widget{size = {W,H}});

handle(redraw, Wd = #widget{state = S, format = F, tags = T, size = {W,H}}) ->
    #state{align = Align, text = Text, bgcolor = Bg, fgcolor = Fg, checked = Chk} = S,
    IdleBg = {0.8, 0.8, 0.8},
    ActiveBg = {1.0, 1.0, 1.0},
    CheckFg = {0.0, 0.0, 0.0},
    Focus = lists:member(focus, T),
    MouseIn = lists:member(mouse_in, T),
    Lines = binary:split(Text, <<"\n">>, [global]),
    NLines = lists:zip(lists:seq(1, length(Lines)), Lines),
    LineH = H / length(Lines),
    BoxW = H,
    TextW = W - BoxW - 10.0,
    Image0 = #cairo_image{width = round(TextW), height = round(H),
        format = F, data = <<>>},
    {ok, Tags, _} = cairerl_nif:draw(Image0, [], [
        #cairo_select_font_face{family= <<"sans-serif">>},
        #cairo_set_font_size{size = 0.8 * LineH},
        #cairo_font_extents{tag = fontext}] ++
        lists:map(fun({N, Line}) ->
            #cairo_text_extents{text = <<Line/binary, 0>>, tag = {textext, N}}
        end, NLines)),
    LineWs = [{N, LW} || {{textext,N}, #cairo_tag_text_extents{width = LW}} <- Tags],
    NLineW = [{N, proplists:get_value(N, NLines), proplists:get_value(N, LineWs)} || N <- lists:seq(1, length(Lines))],
    #cairo_tag_font_extents{height = FontHeight, descent = FontDescent} = proplists:get_value(fontext, Tags),
    {ok, _, Image1} = cairerl_nif:draw(Image0, [], [
        color_set_order(Bg),
        #cairo_rectangle{width=W,height=H},
        #cairo_fill{},

        color_set_order(IdleBg),
        #cairo_rectangle{width=BoxW, height=H},
        #cairo_fill{}
        ] ++
        if MouseIn or Focus -> [
            color_set_order(ActiveBg),
            #cairo_translate{x = 2.0, y = 2.0},
            #cairo_rectangle{width=BoxW - 4.0, height = H - 4.0},
            #cairo_fill{}];
        true -> []
        end ++
        if Chk -> [
            #cairo_identity_matrix{},
            color_set_order(CheckFg),
            #cairo_new_path{},
            #cairo_set_line_width{width = 1.5},
            #cairo_move_to{x = BoxW - 4.0, y = 4.0},
            #cairo_line_to{x = 4.0, y = H - 4.0},
            #cairo_move_to{x = 4.0, y = 4.0},
            #cairo_line_to{x = BoxW - 4.0, y = H - 4.0},
            #cairo_stroke{}];
        true -> []
        end ++ [
        color_set_order(Fg),
        #cairo_select_font_face{family= <<"sans-serif">>},
        #cairo_set_font_size{size = 0.8 * LineH}] ++
        lists:flatmap(fun({N, Line, LWidth}) ->
            [#cairo_new_path{},
             #cairo_identity_matrix{}] ++
            case Align of
                center -> [#cairo_translate{
                    x = BoxW + 10 + TextW/2 - LWidth/2,
                    y = FontHeight * N - FontDescent}];
                left -> [#cairo_translate{
                    x = BoxW + 10,
                    y = FontHeight * N - FontDescent}];
                right -> [#cairo_translate{
                    x = BoxW + 10 + (TextW - LWidth),
                    y = FontHeight * N - FontDescent}]
            end ++
            [#cairo_show_text{text = <<Line/binary, 0>>}]
        end, NLineW)),
    {ok, Wd#widget{size = {W,H}, orders = [
        #image{image = Image1}
    ]}, []};

handle(Event, Wd) ->
    ui:default_handler(Event, Wd).
