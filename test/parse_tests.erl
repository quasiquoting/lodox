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


%%%===================================================================
%%% Custom types
%%%===================================================================

arglist_simple() -> list(atom()).

body() -> union([[printable_string()], [non_string_term() | list(form())]]).

docstring() -> printable_string().

form() -> union([non_string_term(), printable_string(), [ atom() | list()]]).

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
