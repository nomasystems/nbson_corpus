%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(nbson_corpus_gconf).

-type gen_conf() ::
    #{
        max_fields => pos_integer(),
        max_depth => non_neg_integer(),
        max_in_count => non_neg_integer()
    }.

%%% TYPE EXPORTS
-export_type([
    gen_conf/0
]).

%%% API EXPORTS
-export([
    default/0
]).

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
default() ->
    #{
        max_fields => 1,
        max_depth => 0,
        max_in_count => 0
    }.
