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
-module(nbson_corpus).

%%% MACROS
-define(DEFAULT_COUNT, 10).

%%% API EXPORTS
-export([
    generate/0,
    generate/1,
    generate/2,
    generate/3
]).

%%% TYPE SPECS
-type out() :: list(map()).

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate() -> Result when Result :: out().
generate() ->
    generate(?DEFAULT_COUNT).

-spec generate(DocCount) -> Result when
    DocCount :: non_neg_integer(),
    Result :: out().
generate(DocCount) ->
    generate(DocCount, nbson_corpus_gconf:default()).

-spec generate(DocCount, GenConf) -> Result when
    DocCount :: non_neg_integer(),
    GenConf :: nbson_corpus_gconf:gen_conf(),
    Result :: out().
generate(DocCount, GenConf) ->
    GenConf1 = maps:merge(nbson_corpus_gconf:default(), GenConf),
    nbson_corpus_docs:docs(DocCount, GenConf1).

-spec generate(DocCount, GenConf, Function) -> Result when
    DocCount :: non_neg_integer(),
    GenConf :: nbson_corpus_gconf:gen_conf(),
    Function :: fun(),
    Result :: any().
generate(DocCount, GenConf, Function) when is_function(Function) ->
    nbson_corpus_docs:docs(DocCount, GenConf, Function).
