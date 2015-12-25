-module(parse_tests).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 60).

-define(OPTIONS(N), [{on_output, fun pprint/2}, {numtests, N}]).


%%%===================================================================
%%% EUnit tests
%%%===================================================================

parse_test_() ->
  Properties =
    [ {"A function without a docstring produces an empty docstring.",
       prop_defun_simple(), 100}
    , {"A simple function with a docstring is correctly parsed.",
       prop_defun_simple_doc(), 100}
    , {"A function with pattern clauses produces an empty docstring.",
       prop_defun_match(), 30}
    , {"A function with pattern clauses and a docstring is correctly parsed.",
       prop_defun_match_doc(), 30}
    ],
  [{timeout, ?TIMEOUT,
    {Title, ?_assert(proper:quickcheck(Property, ?OPTIONS(NumTests)))}}
   || {Title, Property, NumTests} <- Properties].


%%%===================================================================
%%% Properties
%%%===================================================================

prop_defun_simple() ->
  ?FORALL(D, defun_simple(),
          begin
            {ok, #{doc := Doc}} = 'lodox-parse':'form-doc'(D),
            "" =:= Doc
          end).

prop_defun_simple_doc() ->
  ?FORALL(Defun, defun_simple_doc(),
          begin
            {ok, #{doc := Doc}} = 'lodox-parse':'form-doc'(Defun),
            lists:nth(4, Defun) =:=  Doc
          end).

prop_defun_match() ->
  ?FORALL(Defun, defun_match(),
          begin
            {ok, #{doc := Doc}} = 'lodox-parse':'form-doc'(Defun),
            "" =:= Doc
          end).

prop_defun_match_doc() ->
  ?FORALL(Defun, defun_match_doc(),
          begin
            {ok, #{doc := Doc}} = 'lodox-parse':'form-doc'(Defun),
            lists:nth(3, Defun) =:=  Doc
          end).


%%%===================================================================
%%% defun shapes
%%%===================================================================

defun_simple() ->
  [defun, atom(), arglist_simple()
   | body()].

defun_simple_doc() ->
  [defun, atom(), arglist_simple(),
   docstring()
   | body()].

defun_match() ->
  [defun, atom()
   | non_empty(list(pattern_clause()))].

defun_match_doc() ->
  [defun, atom(),
   docstring()
   | non_empty(list(pattern_clause()))].


%%%===================================================================
%%% Custom types
%%%===================================================================

%%% Functions

arglist_simple() -> list(atom()).

body() -> union([[printable_string()], [non_string_term() | list(form())]]).

form() -> union([non_string_term(), printable_string(), [ atom() | list()]]).

docstring() -> printable_string().

arglist_patterns() -> non_empty(list(pattern())).


%%% Patterns

pattern() -> union([non_string_term(), printable_string(), pattern_form()]).

pattern_form() ->
  [oneof([backquote, quote, binary, list, map, tuple, match_fun()])
   | non_empty(list())].

match_fun() -> ?LET(F, printable_string(), list_to_atom("match-" ++ F)).

pattern_clause() -> [arglist_patterns() | [oneof([guard(), form()]) | body()]].

guard() -> ['when' | non_empty(list(union([logical_clause(), comparison()])))].


%%% Logical clauses

logical_clause() ->
  X = union([atom(), comparison()]),
  [logical_operator(), X | non_empty(list(X))].

logical_operator() -> oneof(['and', 'andalso', 'or', 'orelse']).


%%% Comparisons

comparison() -> [comparison_operator(), atom() | list(atom())].

comparison_operator() -> oneof(['==', '=:=', '=/=', '<', '>', '=<', '>=']).


%%% Strings and non-strings

non_string_term() ->
  union([atom(), number(), [], bitstring(), binary(), boolean(), tuple()]).

printable_char() -> union([integer(32, 126), integer(160, 255)]).

printable_string() -> list(printable_char()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

pprint(_Format, [Defun]) when defun =:= hd(Defun) ->
  io:format(user, "~s~n", [pprint(Defun)]);
pprint(Format, Data) ->
  io:format(user, Format, Data).

pprint(Term) ->
  re:replace(lfe_io_pretty:term(Term), "comma ", ". ,",
             [global, {return, list}]).
