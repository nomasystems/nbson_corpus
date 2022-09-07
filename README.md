# nbson_corpus
![nbson_corpus](https://github.com/nomasystems/nbson_corpus/actions/workflows/build.yml/badge.svg)

`nbson_corpus` is an OTP library to produce randomly generated maps. These maps are to represent [BSON](https://en.wikipedia.org/wiki/BSON) documents and properties get generated in accordance to [nbson](https://github.com/nomasystems/nbson) representation of BSON documents.

With `nbson_corpus` you can generate the Erlang representation of a series of BSONs and then encode them into a BSON binary using `nbson`.

## Setup

```erl
%%% e.g., rebar.config
{deps, [
    {nbson_corpus, {git, "git@github.com:nomasystems/nbson_corpus.git", {branch, "main"}}}
]}.
```

## Features
`nbson_corpus` exposes utilities via its API that allows you to:

| Function | Description |
| -------- | ----------- |
| `nbson_corpus:generate/0,1,2` | Generate a list of maps. `generate/2` expects the number of maps to be generated and a [configuration](#configuration) map. `generate/1` works as `/2` but using a default configuration. `generate/0` works as `/1` but using a default number of maps to be generated.|
| `nbson_corpus:generate/3` | Same as `generate/2` but allows to specify a Fun as third parameter. This Fun is applied to each new proplist on generation, and must return `ok`. |

## Configuration
`nbson_corpus:generate/2,3` functions expect a configuration map to be passed as second parameter. 

The configuration map should have the following keys and values.
 - `max_fields`: maximum number of fields on each generated proplist. A positive integer.
 - `max_depth`: maximum proplist nesting. Even if a proplist is generated as field value or as list field value, this counts as a new level of nesting. A non-negative integer.
 - `max_in_count`: maximum number of elements inside nested maps or list values. A non-negative integer.

## Examples
The following examples show how to generate:
 - A list of 1 map, with 1 maximum field each with no nesting.
    ```erl
    1> nbson_corpus:generate(1, #{max_fields => 1, max_depth => 0}).
    [[{<<"javascript-Zg==">>,
      {javascript,[{}],<<"function(x) { return x; }">>}}]]
    ```
 - A list of 5 maps, of maximum 1 field each and with no nesting.
    ```erl
    2> nbson_corpus:generate(5, #{max_fields => 1, max_depth => 0}).                   
    [[{<<"double-vQ==">>,63553627.25055845}],
     [{<<"undefined-N4U=">>,undefined}],
     [{<<"timestamp-R9Q=">>,{timestamp,1342111817,1339695094}}],
     [{<<"timestamp-wSaTOA==">>,
        {timestamp,1057940720,103957660}}],
     [{<<"date-Qg==">>,{951692044,894125578,1915751174}}]]
    ```
  - A list of 5 maps of 4 fields maximum each with 1 maximum level of nesting, where arrays and embeded documents should have 3 elements or fields respectively at most.
    ```erl
    5> nbson_corpus:generate(5, #{max_fields => 4, max_depth => 1, max_in_count => 3}).
    [[{<<"date-Hdc=">>,{52443542,1746558407,1653013798}},
      {<<"javascript_ws-lxI=">>,
         {javascript,[{<<"x">>,1}],
           <<"function (x){ return x * x; }">>}},
      {<<"timestamp-pkqWuw==">>,{timestamp,1005584595,800316145}},
      {<<"array-Kfe3Lg==">>,
         [2143064143,38680856.06272248,
         {object_id,<<"3767c8736e47">>}]}],
     [{<<"javascript-MpA=">>,
        {javascript,[{}],<<"function(x) { return x; }">>}},
      {<<"object_id-stkF">>,{object_id,<<"cfa14ece79f2">>}}],
     [{<<"min_key-GK4Q+Q==">>,minkey},
      {<<"timestamp-Aw==">>,{timestamp,259892847,746795559}},
      {<<"regular_expression-i8A=">>,
        {regex,<<"mYpBcVrKC/JsdPisYsr2">>,"imxs"}},
      {<<"undefined-zQ==">>,undefined}],
     [{<<"32bit_int-hw==">>,1536841124},
      {<<"decimal-405ylw==">>,62821287.877116635},
      {<<"array-X7U=">>,
        [{data,<<18,0,0,0,1,249,3,46,94,5,42,198,167,219,255,151,
                236,...>>},
         16577005.66228211,
         {data,<<4,0,0,0,1,244,101,120,11>>}]},
      {<<"object-CA==">>,
        [{<<"db_pointer-lzOg2Q==">>,
           {pointer,<<"ZxY=">>,<<"13ea9c5e0107">>}},
         {<<"string-5nJQ">>,<<"V5cQwHsBOzQWJbbutweGJA==">>}]}],
     [{<<"symbol-BzLm3Q==">>,symbol},
      {<<"db_pointer-Yg==">>,
        {pointer,<<"SQp5A2wq1s7JnvSGviGb/Fw=">>,
          <<"c9f39c15dd7c">>}}]]
    ```

You can use combined the `nbson_corpus:generate/3` function and the `nbson:encode` function to produce randomly generated BSON files. Here's how:
```erl
% Having the nbson library loaded:
1> F = fun(X) -> B = nbson:encode(X), file:write_file("example.bson", B, [append]) end.          
#Fun<erl_eval.44.79398840>
2> nbson_corpus:generate(5, #{max_fields => 4, max_depth => 1, max_in_count => 3}, F).            
ok
```

The Fun passed as parameter encodes the proplist passed as paramter as BSON binary and then appends it to an output `example.bson` file. Passing this function as last parameter to `nbson_corpus:generate` applies it to the list of random maps generated.

## Support

Any doubt or suggestion? Please check out [our issue tracker](https://github.com/nomasystems/nbson_corpus/issues).
