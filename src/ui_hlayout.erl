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

-module(ui_hlayout).
-include_lib("rdp_proto/include/rdpp.hrl").
-include("ui.hrl").
-include_lib("cairerl/include/cairerl.hrl").

-export([handle/2]).

-record(state, {margin=20, valign=center}).

do_layout(Kids, {W, H}, #state{margin = Margin, valign = VAlign}) ->
    UsedWidth = lists:sum([KW || #widget{size = {KW, _KH}} <- Kids]) + Margin * (length(Kids) - 1),
    StartX = W / 2 - UsedWidth / 2,
    {_, NewKids} = lists:foldl(fun(Kid = #widget{size = {KW,KH}}, {X, KKids}) ->
        ThisX = if (X =:= StartX) -> X; true -> X + Margin end,
        ThisY = case VAlign of
            center -> H / 2 - KH / 2;
            top -> Margin;
            bottom -> H - Margin - KH
        end,
        KKid = Kid#widget{dest = {ThisX, ThisY}},
        {ThisX + KW, [KKid | KKids]}
    end, {StartX, []}, Kids),
    lists:reverse(NewKids).

handle(init, Wd = #widget{}) ->
    {ok, Wd#widget{state = #state{}}, []};

handle({set_margin, Margin}, Wd = #widget{state = S, size = Sz, children = Kids}) ->
    S2 = S#state{margin = Margin},
    NewKids = do_layout(Kids, Sz, S2),
    {ok, Wd#widget{children = NewKids, orders = [#null_order{}], state = S2}, []};

handle({set_valign, VAlign}, Wd = #widget{state = S, size = Sz, children = Kids}) ->
    S2 = S#state{valign = VAlign},
    NewKids = do_layout(Kids, Sz, S2),
    {ok, Wd#widget{children = NewKids, orders = [#null_order{}], state = S2}, []};

handle({children_updated, _OldKids}, Wd = #widget{state = S, size = Sz, children = Kids}) ->
    NewKids = do_layout(Kids, Sz, S),
    case NewKids of
        Kids -> {ok, Wd, []};
        _ -> {ok, Wd#widget{children = NewKids, orders = [#null_order{}]}, []}
    end;

handle({resize, NewSize}, Wd = #widget{state = S, children = Kids}) ->
    NewKids = do_layout(Kids, NewSize, S),
    {ok, Wd#widget{children = NewKids, state = S, size = NewSize, orders = [#null_order{}]}, []};

handle(Event, Wd) ->
    ui:default_handler(Event, Wd).
