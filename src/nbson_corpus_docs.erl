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
-module(nbson_corpus_docs).

%%% EXTERNAL EXPORTS
-export([
    docs/2,
    docs/3
]).

%%% MACROS
-define(TYPES, nbson_corpus_conf:types()).
-define(NO_DEEP_TYPES, nbson_corpus_conf:types() -- [3, 4]).
-define(TYPES_AMOUNT, nbson_corpus_conf:num_types()).

-define(MAX_FLOAT, 100000000).
-define(MAX_INT, 2147483647).
-define(MAX_LONG, 9223372036854775807).
-define(MAX_STRING_SIZE, 20).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
docs(Count, Conf) ->
    docs(Count, Conf, []).

docs(0, _Conf, Docs) when is_list(Docs) ->
    Docs;
docs(Count, Conf, Docs) when is_list(Docs), is_integer(Count), Count > 0 ->
    docs(Count - 1, Conf, [doc(Conf) | Docs]);
docs(0, _Conf, Fun) when is_function(Fun) ->
    ok;
docs(Count, Conf, Fun) when is_function(Fun), is_integer(Count), Count > 0 ->
    Fun(doc(Conf)),
    docs(Count - 1, Conf, Fun).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
doc(#{max_fields := MaxF, max_depth := MaxD} = Conf) ->
    FieldCount = rand:uniform(MaxF),
    fields(FieldCount, Conf#{max_depth := MaxD - 1}, #{}).

fields(0, _Conf, Fields) ->
    Fields;
fields(FieldCount, Conf, Fields) ->
    {K, V} = field(Conf),
    fields(FieldCount - 1, Conf, maps:put(K, V, Fields)).

field(#{max_depth := N} = Conf) when N < 0 ->
    Type = rand_type(no_deep),
    {field_key(Type), field_value(Type, Conf)};
field(Conf) ->
    Type = rand_type(),
    {field_key(Type), field_value(Type, Conf)}.

field_key(Type) ->
    <<(type_str(Type))/binary, <<"-">>/binary, (rand_string(4))/binary>>.

field_value(1, _Conf) ->
    rand:uniform(?MAX_FLOAT) + rand:uniform();
field_value(2, _Conf) ->
    rand_string();
field_value(3, #{max_in_count := MaxInCount} = Conf) ->
    doc(Conf#{max_fields := MaxInCount});
field_value(4, Conf) ->
    rand_array(Conf);
field_value(5, _Conf) ->
    rand_binary_data();
field_value(6, _Conf) ->
    undefined;
field_value(7, _Conf) ->
    rand_object_id();
field_value(8, _Conf) ->
    rand_boolean();
field_value(9, _Conf) ->
    rand_date();
field_value(10, _Conf) ->
    null;
field_value(11, _Conf) ->
    rand_regex();
field_value(12, _Conf) ->
    rand_db_pointer();
field_value(13, _Conf) ->
    rand_javascript();
field_value(14, _Conf) ->
    rand_symbol();
field_value(15, _Conf) ->
    rand_javascript_ws();
field_value(16, _Conf) ->
    rand_int();
field_value(17, _Conf) ->
    rand_timestamp();
field_value(18, _Conf) ->
    rand_long();
field_value(19, _Conf) ->
    rand_decimal();
field_value(-1, _Conf) ->
    rand_minkey();
field_value(127, _Conf) ->
    rand_maxkey().

rand_type() ->
    lists:nth(rand:uniform(?TYPES_AMOUNT - 1), ?TYPES).

rand_type(no_deep) ->
    lists:nth(rand:uniform(?TYPES_AMOUNT - 3), ?NO_DEEP_TYPES).

rand_string() ->
    rand_string(?MAX_STRING_SIZE).

rand_string(N) ->
    base64:encode(crypto:strong_rand_bytes(rand:uniform(N))).

rand_array(#{max_in_count := 0}) ->
    [];
rand_array(#{max_in_count := MaxInCount, max_depth := MaxD} = Conf) when MaxD =< 1 ->
    [
        field_value(rand_type(no_deep), Conf#{max_depth := MaxD - 1})
     || _N <- lists:seq(1, MaxInCount)
    ];
rand_array(#{max_in_count := MaxInCount, max_depth := MaxD} = Conf) ->
    [
        field_value(rand_type(), Conf#{max_depth := MaxD - 1})
     || _N <- lists:seq(1, MaxInCount)
    ].

rand_binary_data() ->
    Bin = crypto:strong_rand_bytes(rand:uniform(?MAX_STRING_SIZE)),
    Size = erlang:byte_size(Bin),
    {data, <<(Size):32/little-signed, (1):8/little, Bin/binary>>}.

rand_object_id() ->
    {object_id, rand_id()}.

rand_id() ->
    list_to_binary(
        string:lowercase(
            lists:flatten([integer_to_list(B1, 16) || <<B1:4>> <= crypto:strong_rand_bytes(6)])
        )
    ).

rand_boolean() ->
    rand_boolean(rand:uniform(2)).

rand_boolean(1) ->
    true;
rand_boolean(_) ->
    false.

rand_date() ->
    {rand_int(), rand_int(), rand_int()}.

rand_regex() ->
    {regex, rand_string(), "imxs"}.

rand_db_pointer() ->
    {pointer, rand_string(), rand_id()}.

rand_javascript() ->
    {javascript, [{}], <<"function(x) { return x; }">>}.

rand_javascript_ws() ->
    {javascript, [{<<"x">>, 1}], <<"function (x){ return x * x; }">>}.

rand_symbol() ->
    symbol.

rand_int() ->
    rand:uniform(?MAX_INT).

rand_timestamp() ->
    {timestamp, rand_int(), rand_int()}.

rand_long() ->
    rand:uniform(?MAX_LONG).

rand_decimal() ->
    rand:uniform(?MAX_FLOAT) + rand:uniform().

rand_minkey() ->
    minkey.

rand_maxkey() ->
    maxkey.

type_str(1) ->
    <<"double">>;
type_str(2) ->
    <<"string">>;
type_str(3) ->
    <<"object">>;
type_str(4) ->
    <<"array">>;
type_str(5) ->
    <<"binary_data">>;
type_str(6) ->
    <<"undefined">>;
type_str(7) ->
    <<"object_id">>;
type_str(8) ->
    <<"boolean">>;
type_str(9) ->
    <<"date">>;
type_str(10) ->
    <<"null">>;
type_str(11) ->
    <<"regular_expression">>;
type_str(12) ->
    <<"db_pointer">>;
type_str(13) ->
    <<"javascript">>;
type_str(14) ->
    <<"symbol">>;
type_str(15) ->
    <<"javascript_ws">>;
type_str(16) ->
    <<"32bit_int">>;
type_str(17) ->
    <<"timestamp">>;
type_str(18) ->
    <<"64bit_int">>;
type_str(19) ->
    <<"decimal">>;
type_str(-1) ->
    <<"min_key">>;
type_str(127) ->
    <<"max_key">>.
