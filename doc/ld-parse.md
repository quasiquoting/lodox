- [docs/0](#sec-1)
- [docs/1](#sec-2)
- [docs/2](#sec-3)
- [to-org/1](#sec-4)
- [to-org/2](#sec-5)

# docs/0<a id="sec-1" name="sec-1"></a>

```lfe
()
```

    TODO: write docstring

# docs/1<a id="sec-2" name="sec-2"></a>

```lfe
(file-or-dir)
```

    Given a path to an LFE file or a directory containing LFE files,
    return a map from module name to orddict from fun/arity to a property map.

# docs/2<a id="sec-3" name="sec-3"></a>

```lfe
(file dir)
```

    Given a filename, `file`, and a directory, `dir`, call #'docs/1 on `(filename:join dir file)`.

# to-org/1<a id="sec-4" name="sec-4"></a>

```lfe
(dict)
```

    TODO: write docstring
    
    Project level.

# to-org/2<a id="sec-5" name="sec-5"></a>

```lfe
(dict filename)
```

    TODO: write docstring
    
    Module level.
