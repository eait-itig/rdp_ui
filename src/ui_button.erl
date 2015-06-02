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

-module(ui_button).
-include_lib("rdp_proto/include/rdpp.hrl").
-include("ui.hrl").
-include_lib("cairerl/include/cairerl.hrl").

-export([handle/2]).

-record(state, {text}).

color_set_order({R, G, B}) ->
    #cairo_set_source_rgba{r = float(R), g = float(G), b = float(B)}.

handle({init, Text}, Wd = #widget{size = Sz}) ->
    handle({resize, Sz}, Wd#widget{state = #state{text = Text}});

handle({set_text, Text}, Wd = #widget{state = S, size = Sz}) ->
    S2 = S#state{text = Text},
    handle({resize, Sz}, Wd#widget{state = S2});

handle(#ts_inpevt_mouse{action = down, buttons = [1]}, Wd = #widget{tags = T, size = Sz}) ->
    case lists:member(mouse_down, T) of
        true -> {ok, Wd, []};
        false -> handle({resize, Sz}, Wd#widget{tags = [mouse_down | T]})
    end;

handle(#ts_inpevt_mouse{action = up, buttons = [1]}, Wd = #widget{tags = T, size = Sz}) ->
    case lists:member(mouse_down, T) of
        true ->
            {ok, Wd2, Evts} = handle({resize, Sz}, Wd#widget{tags = T -- [mouse_down]}),
            {ok, Wd2, Evts ++ [{ ui, {clicked, Wd2#widget.id} }]};
        false -> {ok, Wd, []}
    end;

handle(#ts_inpevt_mouse{action = move}, Wd = #widget{tags = T, size = Sz}) ->
    case lists:member(mouse_in, T) of
        true -> {ok, Wd, []};
        false -> handle({resize, Sz}, Wd#widget{tags = [mouse_in | T]})
    end;

handle(mouse_out, Wd = #widget{tags = T, size = Sz}) ->
    case T of
        [] -> {ok, Wd, []};
        _ -> handle({resize, Sz}, Wd#widget{tags = T -- [mouse_in, mouse_down]})
    end;

handle({resize, {W,H}}, Wd = #widget{state = S, tags = T, format = F}) ->
    MouseIn = lists:member(mouse_in, T),
    IdleBg = {16#78 / 256, 16#1a / 256, 16#97 / 256},
    ActiveBg = {16#99 / 256, 16#2a / 256, 16#c1 /256},
    Fg = {0.95,0.95,1},
    #state{text = Text} = S,
    Image0 = #cairo_image{width = round(W), height = round(H),
        format = F, data = <<>>},
    {ok, _, Image1} = cairerl_nif:draw(Image0, [], [
        color_set_order(IdleBg),
        #cairo_rectangle{width=W,height=H},
        #cairo_fill{}] ++
        if MouseIn -> [
            color_set_order(ActiveBg),
            #cairo_translate{x = 2.0, y = 2.0},
            #cairo_rectangle{width = W - 4.0, height = H - 4.0},
            #cairo_fill{}];
        not MouseIn -> []
        end ++ [
        #cairo_identity_matrix{},

        color_set_order(Fg),
        #cairo_select_font_face{family= <<"sans-serif">>},
        #cairo_set_font_size{size = 0.4 * H},
        #cairo_font_extents{tag=fonte},
        #cairo_text_extents{text = Text, tag = txte},
        #cairo_tag_deref{tag=txte, field=width, out_tag=txtw},
        #cairo_tag_deref{tag=txte, field=height, out_tag=txth},
        #cairo_tag_deref{tag=fonte, field=height, out_tag=fonth},
        #cairo_tag_deref{tag=fonte, field=descent, out_tag=fontdec},
        #cairo_translate{x = W / 2, y = H / 2},
        #cairo_scale{x = -0.5, y = 0.5},
        #cairo_translate{x = txtw, y = fonth},
        #cairo_scale{x = -2.0, y = -2.0},
        #cairo_translate{y = fontdec},
        #cairo_scale{y = -1.0},
        #cairo_show_text{text = Text}
    ]),
    {ok, Wd#widget{size = {W,H}, orders = [
        #image{image = Image1}
    ]}, []};

handle(Event, Wd) ->
    ui:default_handler(Event, Wd).
