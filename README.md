[![img](https://travis-ci.org/quasiquoting/lodox.svg)](https://travis-ci.org/quasiquoting/lodox)
[![img](https://badge.fury.io/gh/quasiquoting%2Flodox.svg)](https:/github.com/quasiquoting/lodox/releases/latest)
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
