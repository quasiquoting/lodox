[![img](https://travis-ci.org/lfe-rebar3/lodox.svg)](https://travis-ci.org/lfe-rebar3/lodox)
![img](https://img.shields.io/github/tag/lfe-rebar3/lodox.svg)
[![img](https://img.shields.io/badge/erlang-%E2%89%A518.0-red.svg)](http://www.erlang.org/downloads)
[![img](https://img.shields.io/badge/docs-91%25-green.svg)](http://lfe-rebar3.github.io/lodox)
[![img](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

# Introduction

Like [Codox](https://github.com/weavejester/codox) for [LFE](https://github.com/rvirding/lfe). Check out the [self-generated documentation](http://lfe-rebar3.github.io/lodox/).

Requires Erlang 18.x or later.

# Installation

First, make sure you have the [lfe-compile](https://github.com/lfe-rebar3/compile) plugin as a dependency in your
project's `rebar.config`:

```erlang
{plugins,
 [{'lfe-compile',
   {git, "git://github.com/lfe-rebar3/compile.git",
    {tag, "0.3.0"}}}]}
```

Then in your project's `rebar.config`, include the [provider pre-hook](https://www.rebar3.org/v3.0/docs/configuration#section-provider-hooks):

```erlang
{provider_hooks,
 [{pre, [{compile, {lfe, compile}}]}]}
```

Finally, add Lodox to your `project_plugins` list.

```erlang
{project_plugins,
 [% ...
  {lodox,
   {git, "git://github.com/lfe-rebar3/lodox.git",
    {tag, "0.12.12"}}}]}.
```

# Usage

In order for Lodox to work, your project must first be compiled:

```sh
rebar3 compile
```

Then, to invoke Lodox, simply run:

```sh
rebar3 lfe lodox
```

Alternatively, you can `do` both at once:

```sh
rebar3 compile, lfe lodox
```

If all goes well, the output will look something like:

    Generated lodox vX.Y.Z docs in /path/to/lodox/doc

And, as promised, [generated documentation](http://lfe-rebar3.github.io/lodox) will be in the `doc` subdirectory of
your project.

## Source Links

*[ Modified from [Codox documentation](https://github.com/weavejester/codox#source-links). ]*

If you have the source available at a URI and would like to have links to the
function/macro's source file in the documentation, you can set the `​'source-uri'​`
[configuration parameter](http://www.erlang.org/doc/design_principles/applications.html#id76014) in your [application resource file](http://www.erlang.org/doc/design_principles/applications.html#id75484).

```erlang
{env,
 [{'source-uri',
   "https://github.com/foo/bar/blob/{version}/{filepath}#L{line}"}]}
```

The URI is a template that may contain the following keys:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">filepath</td>
<td class="org-left">the file path from the root of the repository</td>
</tr>


<tr>
<td class="org-left">line</td>
<td class="org-left">the line number of the source file</td>
</tr>


<tr>
<td class="org-left">version</td>
<td class="org-left">the version of the project</td>
</tr>
</tbody>
</table>

N.B. In order for `{version}` to work properly, you must add the corresponding
tag. For example, if your `.app` file contains `{vsn, "1.2.3"}` you must add the
tag, `​"1.2.3"​`, to your repo.

## Docstring Formats

*[ Modified from [Codox documentation](https://github.com/weavejester/codox#docstring-formats). ]*

By default, docstrings are rendered by Lodox as Markdown via [pandoc](http://pandoc.org). If `pandoc`
is not available, Lodox will fall back to [erlmarkdown](https://github.com/erlware/erlmarkdown).

It is strongly recommended that you install [pandoc](http://pandoc.org), as it is much more robust.

In a future version, you will be able to override this behaviour by specifying
an explicit format for your docstrings.

Markdown docstrings also support wikilink-style relative links, for referencing
other definitions. Definitions in the current module will be matched first, and
then Lodox will try to find a best match out of all the definitions it's
documenting.

N.B. Module-less definitions in `.lfe` files in the `include` directory,
e.g. [lodox-macros](include/lodox-macros.lfe), will also be included in the search.

```lfe
(defun bar (x)
  "See [[foo/2]] and [[baz:square/1]] for other examples."
  ...)
```

# License

Lodox is licensed under [the MIT License](http://yurrriq.mit-license.org).

```text
The MIT License (MIT)
Copyright © 2015-2016 Eric Bailey <quasiquoting@gmail.com>

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
