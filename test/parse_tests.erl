-module(parse_tests).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 60).

-define(OPTIONS, [{to_file, user}]).


%%%===================================================================
%%% EUnit tests
%%%===================================================================

parse_test_() ->
  Properties =
    [ {"A function without a docstring produces an empty docstring.",
       prop_defun_simple()}
    , {"A simple function with a docstring is correctly parsed.",
       prop_defun_simple_doc()}
    ],
  [{timeout, ?TIMEOUT, {Title, ?_assert(proper:quickcheck(Property, ?OPTIONS))}}
   || {Title, Property} <- Properties].


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
            'lodox-p':'string?'(Doc)
          end).


%%%===================================================================
%%% defun shapes
%%%===================================================================

defun_simple() ->
  [defun, atom(), simple_arglist()
   | non_empty(list(form()))].

defun_simple_doc() ->
  [defun, atom(), simple_arglist(),
   docstring()
   | non_empty(list(form()))].


%%%===================================================================
%%% Custom types
%%%===================================================================

simple_arglist() -> list(atom()).

docstring() -> non_empty(list(printable_char())).

form() -> non_empty(list()).

printable_char() -> union([integer(32, 126), integer(160, 255)]).
