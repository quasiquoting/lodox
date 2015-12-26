[![img](https://travis-ci.org/quasiquoting/lodox.svg)](https://travis-ci.org/quasiquoting/lodox)
[![img](//badge.fury.io/gh/quasiquoting%2Flodox.svg)](https://github.com/quasiquoting/lodox/releases/latest)
[![img](https://img.shields.io/github/license/quasiquoting/lodox.svg)](LICENSE)

Current version:

    0.5.0

# Introduction

Like [Codox](https://github.com/weavejester/codox) for [LFE](https://github.com/rvirding/lfe). Check out the [self-generated documentation](http://quasiquoting.org/lodox/).

```erlang
{application,    'lodox',
 [{description,  "The LFE rebar3 Lodox plugin"},
  {vsn,          "0.5.0"},
  {modules,      ['lodox-html-writer','lodox-org-writer',
                  'lodox-p','lodox-parse','lodox-util',
                  lodox,
                  'unit-lodox-tests']},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {env,
   [{'source-uri',
     "https://github.com/quasiquoting/lodox/blob/master/{filepath}#L{line}"}]}]}.
```

# Installation

First, make sure you have the [lfe-compile](https://github.com/lfe-rebar3/compile) plugin as a dependency in your
project's `rebar.config` or, better yet, in the the global [rebar3](https://github.com/rebar/rebar3) config, `~/.config/rebar3/rebar.config`:

```erlang
{plugins,
 [{'lfe-compile', ".*",
   {git, "git://github.com/lfe-rebar3/compile.git",
    {tag, "0.2.0"}}}]}
```

Then in your project's `rebar.config`, include the [provider pre-hook](https://www.rebar3.org/v3.0/docs/configuration#section-provider-hooks):

```erlang
{provider_hooks,
 [{pre, [{compile, {lfe, compile}}]}]}
```

Finally, add Lodox to your `plugins` list:

```erlang
{plugins,
 [% ...
  {lodox, ".*",
   {git, "git://github.com/quasiquoting/lodox.git",
    {tag, "0.5.0"}}}]}.
```

The recommended place for the Lodox plugin entry is the global [rebar3](https://github.com/rebar/rebar3) config, `~/.config/rebar3/rebar.config`,
but it works at the project level, too.

# Usage

In order for Lodox to work, your project must first be compiled:

```sh
rebar3 compile
```

Then, to invoke Lodox, simply run:

```sh
rebar3 lodox
```

Alternatively, you can `do` both at once:

```sh
rebar3 do compile, lodox
```

If all goes well, the output will look something like:

    Generated lodox v0.5.0 docs in /path/to/lodox/doc

And, as promised, [generated documentation](http://quasiquoting.org/lodox/) will be in the `doc` subdirectory of
your project.

Optionally, you can add Lodox as a `compile` [post-hook](https://www.rebar3.org/v3.0/docs/configuration#section-provider-hooks):

```erlang
{provider_hooks,
 [{pre,  [{compile, {lfe, compile}}]},
  {post, [{compile, lodox}]}]}.
```

# Rebar3 Configuration

**Describe `rebar.config` here.**

```erlang
{erl_opts, [debug_info, {src_dirs, ["test"]}]}.

{eunit_compile_opts, [{src_dirs, ["test"]}]}.

{provider_hooks, [{pre, [{compile, {lfe, compile}}]}]}.
```

The first and foremost dependency is, of course, [LFE](https://github.com/rvirding/lfe) itself.
Use the latest version, which as of this writing, is:

    0.10.1

To make writing [EUnit](http://www.erlang.org/doc/apps/eunit/chapter.html) tests easier, use [ltest](https://github.com/lfex/ltest).

    0.7.0

To handle HTML rendering, use [exemplar](https://github.com/lfex/exemplar).

N.B. I'm only using [my fork](https://github.com/yurrriq/exemplar) until [this pull request](https://github.com/lfex/exemplar/pull/15) or something similar
gets merged into the [lfex](https://github.com/lfex) repo.

    0.3.0

For markdown: [erlmarkdown](https://github.com/erlware/erlmarkdown).

```erlang
{deps,
 [{lfe,      {git, "git://github.com/rvirding/lfe.git", {tag, "0.10.1"}}},
  {ltest,    {git, "git://github.com/lfex/ltest.git", {tag, "0.7.0"}}},
  {exemplar, {git, "git://github.com/yurrriq/exemplar.git", {tag, "0.3.0"}}},
  {markdown, {git, "git://github.com/erlware/erlmarkdown.git"}},
  {proper,
   {git, "git://github.com/quasiquoting/proper.git",
    {branch, "master"}}}]}.
```

# Modules

## [lodox](src/lodox.lfe)

```lfe
(defmodule lodox
  (doc "The Lodox [Rebar3][1] [provider][2].

[1]: http://www.rebar3.org/docs/plugins
[2]: https://github.com/tsloughter/providers ")
  (behaviour provider)
  (export all))
```

### [Provider Interface](http://www.rebar3.org/v3.0/docs/plugins#section-provider-interface)

-   *namespace*: in which the provider is registered.
    In this case, use `default`, which is the main namespace.

```lfe
(defun namespace     () 'default)
```

-   *name*: The 'user friendly' name of the task.

```lfe
(defun provider-name () 'lodox)
```

-   *short​\_desc*: A one line short description of the task, used in lists of
    providers.

```lfe
(defun short-desc    () "Generate documentation from LFE source files.")
```

-   *deps*: The list of dependencies, providers, that need to run before this
    one. You do not need to include the dependencies of your dependencies.

```lfe
(defun deps          () '(#(default app_discovery)))
```

-   *desc*: The description for the task, used by `rebar3 help`.

```lfe
(defun desc          () (short-desc))
```

`init/1` is called when `rebar3` first boots and simply initiates the provider
and sets up the state.

```lfe
(defun init (state)
  "Initiate the Lodox provider."
  (rebar_api:debug "Initializing {default, lodox}" '())
  (let* ((opts `(#(name       ,(provider-name)) ; The 'user friendly' name
                 #(module     ,(MODULE))        ; The module implementation
                 #(namespace  ,(namespace))     ; Plugin namespace
                 #(opts       ())               ; List of plugin options
                 #(deps       ,(deps))          ; The list of dependencies
                 #(example    "rebar3 lodox")   ; How to use the plugin
                 #(short_desc ,(short-desc))    ; A one-line description
                 #(desc       ,(desc))          ; A longer description
                 #(bare       true)))           ; Task can be run by user
         (provider (providers:create opts)))
    (let ((state* (rebar_state:add_provider state provider)))
      (rebar_api:debug "Initialized lodox" '())
      `#(ok ,state*))))
```

`do/1` parses the rebar state for the `current_app` (as a singleton list) or the
list of `project_apps` and calls `write-docs/1` on each one. This is where the
actual work happens.

```lfe
(defun do (state)
  "Generate documentation for each application in the proejct."
  (rebar_api:debug "Starting do/1 for lodox" '())
  (let ((apps (case (rebar_state:current_app state)
                ('undefined (rebar_state:project_apps state))
                (apps-info   `(,apps-info)))))
    (lists:foreach #'write-docs/1 apps))
  `#(ok ,state))
```

`format_error/1` prints errors when they happen. The point is to enable
filtering of sensitive elements from the state, but in this case, it simply
prints the `reason`.

```lfe
(defun format_error (reason)
  "When an exception is raised or a value returned as
`#(error #((MODULE) reason)`, `(format_error reason)` will be called
so a string can be formatted explaining the issue."
  (io_lib:format "~p" `(,reason)))
```

### Internal Functions

`write-docs/1` takes an `app_info_t` (see: [rebar​\_app​\_info.erl](https://github.com/rebar/rebar3/blob/master/src/rebar_app_info.erl)) and generates
documentation for it.

```lfe
(defun write-docs (app-info)
  (let* ((`(,opts ,app-dir ,name ,vsn ,out-dir)
          (lists:map (lambda (f) (call 'rebar_app_info f app-info))
                     '(opts dir name original_vsn out_dir)))
         (ebin-dir (filename:join out-dir "ebin"))
         (doc-dir  (filename:join app-dir "doc")))
    (rebar_api:debug "Adding ~p to the code path" `(,ebin-dir))
    (code:add_path ebin-dir)
    (let ((project (lodox-parse:docs name))
          (opts    `#m(output-path ,doc-dir app-dir ,app-dir)))
      (rebar_api:debug "Generating docs for ~p" `(,(mref project 'name)))
      (lodox-html-writer:write-docs project opts))
    (generated name vsn doc-dir)))
```

`generated/3` takes an app `name`, `vsn` and output directory and prints a line
describing the docs that were generated.

```lfe
(defun generated
  ([name `#(cmd ,cmd) doc-dir]
   (generated name (os:cmd (++ cmd " | tr -d \"\\n\"")) doc-dir))
  ([name vsn doc-dir]
   (rebar_api:console "Generated ~s v~s docs in ~s" `(,name ,vsn ,doc-dir))))
```

## [lodox-p](src/lodox-p.lfe)

```lfe
(defmodule lodox-p
  (export (clauses? 1) (clause? 1)
          (arglist? 1) (arg? 1)
          (string? 1)))

(defun clauses? (forms)
  "Return `true` iff `forms` is a list of items that satisfy [[clause?/1]]."
  (lists:all #'clause?/1 forms))

(defun clause?
  "Given a term, return `true` iff the it is a list whose head satisfies [[arglist?/1]]."
  ([`(,_)]      'false)
  ([`([] . ,_)] 'false)
  ([`(,h . ,_)] (lodox-p:arglist? h))
  ([_]          'false))

(defun arglist?
  "Given a term, return `true` iff it is either the empty list or a list
containing only items that satisfy [`arg?/1`](#func-arg.3F)."
  (['()]                      'true)
  ([lst] (when (is_list lst)) (lists:all #'arg?/1 lst))
  ([_]                        'false))

(defun arg?
  "Return `true` iff `x` seems like a valid item in an arglist."
  ([(= x `(,h . ,_t))]
   (orelse (string? x)
           (lists:member h '(= () backquote quote binary list map tuple))
           (andalso (is_atom h) (lists:prefix "match-" (atom_to_list h)))))
  ([x]
   (lists:any (lambda (p) (funcall p x))
              (list #'is_atom/1
                    #'is_binary/1
                    #'is_bitstring/1
                    #'is_number/1
                    #'is_map/1
                    #'is_tuple/1
                    #'string?/1))))

(defun string? (data)
  "Return `true` iff `data` is a flat list of printable characters."
  (io_lib:printable_list data))
```

## [lodox-util](src/lodox-util.lfe)

```lfe
(defmodule lodox-util
  (doc "Utility functions to inspect the current version of lodox and its dependencies.")
  (export (search-funcs 2) (search-funcs 3)))

(defun search-funcs (modules partial-func)
  "TODO: write docstring"
  (search-funcs modules partial-func 'undefined))

(defun search-funcs (modules partial-func starting-mod)
  "TODO: write docstring"
  (let* ((suffix  (if (lists:member #\/ partial-func)
                    partial-func
                    `(#\/ . ,partial-func)))
         (matches (lists:filter
                    (lambda (func-name) (lists:suffix suffix func-name))
                    (exported-funcs modules))))
    (case (lists:dropwhile
           (lambda (func-name)
             (=/= (atom_to_list starting-mod) (module func-name)))
           matches)
      (`(,func . ,_) func)
      ('()           (case matches
                       (`(,func . ,_) func)
                       ('()           'undefined))))))
```

```lfe
(defun exported-funcs (modules)
  "TODO: write docstring"
  (lc ((<- mod modules)
       (<- func (mref mod 'exports)))
    (func-name mod func)))

(defun func-name (mod func)
  "TODO: write docstring"
  (++ (atom_to_list (mref mod 'name))
      ":" (atom_to_list (mref func 'name))
      "/" (integer_to_list (mref func 'arity))))

(defun module (func-name)
  (lists:takewhile (lambda (c) (=/= c #\:)) func-name))
```

# Unit Tests

```lfe
(defmodule unit-lodox-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
```

## `project` Shapes

```lfe
(deftestgen projects-shapes
  (lists:zipwith #'validate_project/2 (src-dirs) (all-docs)))

;; EUnit gets very upset if the following _ is a -.
(defun validate_project (dir project)
  `[#(#"project is a map"
      ,(_assert (is_map project)))
    #(#"description is a string"
      ,(_assert (lodox-p:string? (mref* project 'description))))
    #(#"documents is a list"
      ,(_assert (is_list (mref* project 'documents))))
    #(#"modules is a list"
      ,(_assert (is_list (mref* project 'modules))))
    #(#"name matches directory"
      ,(_assertEqual (project-name dir) (mref* project 'name)))
    #(#"version is a list"
      ,(_assert (is_list (mref* project 'version))))])
```

## `modules` Shapes

```lfe
(deftestgen modules-shapes
  (lists:map #'validate_module/1 (project-wide 'modules)))

(defun validate_module (module)
  `[#(#"module is a map"
      ,(_assert (is_map module)))
    #(#"module has correct keys"
      ,(_assertEqual '(behaviour doc exports filepath name) (maps:keys module)))
    #(#"behaviour is a list of atoms"
      ,(_assert (lists:all #'is_atom/1 (mref* module 'behaviour))))
    #(#"doc is a list"
      ,(_assert (is_list (mref* module 'doc))))
    #(#"exports is a list"
      ,(_assert (is_list (mref* module 'exports))))
    #(#"filepath refers to a regular file"
      ,(_assert (filelib:is_regular (mref* module 'filepath))))
    #(#"name is an atom"
      ,(_assert (is_atom (mref* module 'name))))])
```

## `exports` Shapes

```lfe
(deftestgen exports-shapes
  (lists:map #'validate_exports/1 (project-wide 'exports 'modules)))

(defun validate_exports (exports)
  `[#(#"exports is a map"
      ,(_assert (is_map exports)))
    #(#"exports has correct keys"
      ,(_assertEqual '(arglists arity doc line name) (maps:keys exports)))
    #(#"arglists is a list of arglists (which may end with a guard)"
      ,(let ((arglists (lists:map
                         (lambda (arglist)
                           (lists:filter
                             (match-lambda
                               ([`(when . ,_t)] 'false)
                               ([_]             'true))
                             arglist))
                         (mref* exports 'arglists))))
         (_assert (lists:all #'lodox-p:arglist?/1 arglists))))
    #(#"artity is an integer"
      ,(_assert (is_integer (mref* exports 'arity))))
    #(#"doc is a string"
      ,(_assert (lodox-p:string? (mref* exports 'doc))))
    #(#"line is an integer"
      ,(_assert (is_integer (mref* exports 'line))))
    #(#"name is an atom"
      ,(_assert (is_atom (mref* exports 'name))))])
```

# [Travis CI](https://travis-ci.org/quasiquoting/lodox)

```yaml
language: erlang
# http://stackoverflow.com/a/24600210/1793234
# Handle git submodules yourself
git:
  submodules: false
# Use sed to replace the SSH URL with the public URL, then initialize submodules
before_install:
  - sed -i 's/git@github.com:/https:\/\/github.com\//' .gitmodules
  - git submodule update --init --recursive
install: true
before_script:
    - wget https://s3.amazonaws.com/rebar3/rebar3
    - chmod 755 rebar3
script:
  - ./rebar3 eunit -v
notifications:
  email:
    - quasiquoting@gmail.com
otp_release:
  - 18.2
  - 18.0
```

# Literate Programming Setup

Set [`org-confirm-babel-evaluate`](http://orgmode.org/manual/Code-evaluation-security.html#index-org_002dconfirm_002dbabel_002devaluate-2148) to a `lambda` expression that takes the
`lang`-uage and `body` of a code block and returns `nil` if `lang` is
`​"emacs-lisp"​`, otherwise `t`.

```lisp
(setq-local org-confirm-babel-evaluate
            (lambda (lang body)
              (not (string= lang "emacs-lisp"))))
```

Define an Emacs Lisp code block called `generated` that takes a `lang`-uage
(default: `​""​`) and produces a commented notice that source code in this project
is generated by this Org file.

```lisp
(let ((comment (cond
                ((string= lang "erlang") "%%%")
                ((string= lang "yaml")   "###")
                (t                       ";;;")))
      (line    (make-string 67 ?=))
      (warning "This file was generated by Org. Do not edit it directly.")
      (how-to  "Instead, edit Lodox.org in Emacs and call org-babel-tangle."))
  (format "%s%s\n%s %s\n%s %s\n%s%s\n\n"
          comment line
          comment warning
          comment how-to
          comment line))
```

For example, `<<generated("lfe")>>` produces:

```text
;;;===================================================================
;;; This file was generated by Org. Do not edit it directly.
;;; Instead, edit Lodox.org in Emacs and call org-babel-tangle.
;;;===================================================================
```

## License

Lodox is licensed under [the MIT License](http://yurrriq.mit-license.org).

```text
The MIT License (MIT)
Copyright © 2015 Eric Bailey <quasiquoting@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```

Significant code and inspiration from [Codox](https://github.com/weavejester/codox). Copyright © 2015 James Revees

Codox is distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
