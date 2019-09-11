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

-type color() :: {Red :: float(), Green :: float(), Blue :: float()}.
-type point() :: {X :: float(), Y :: float()}.
-type size() :: {Width :: float(), Height :: float()}.

-record(rect, {dest = {0.0, 0.0} :: point(), size :: size(), color :: color()}).
-record(image, {dest = {0.0, 0.0} :: point(), image :: cairerl:image()}).
-record(null_order, {ref = make_ref()}).
-type order() :: #rect{} | #image{} | #null_order{}.

-record(widget, {id :: term(),
                 tags = [] :: [atom()],
                 dest = {0.0, 0.0} :: point(),
                 size = {0.0, 0.0} :: size(),
                 format = rgb24 :: cairerl:pixel_format(),
                 mod = error(no_module) :: atom(),
                 state :: term(),
                 orders = [] :: [order()],
                 children = []}).
-type widget() :: #widget{}.
