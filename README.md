<h1 align="center">
    <a href="https://github.com/rubik/argon">
        Argon
    </a>
</h1>

<p align="center">
    <a href="https://github.com/rubik/argon/actions/workflows/ci.yml">
        <img alt="Tests"
             src="https://github.com/rubik/argon/actions/workflows/ci.yml/badge.svg">
    </a>
    <a href="https://github.com/rubik/argon/blob/master/LICENSE">
        <img alt="License"
             src="https://img.shields.io/badge/license-ISC-blue.svg?style=flat-square">
    </a>
    <a href="https://hackage.haskell.org/package/argon">
        <img alt="Version"
             src="https://img.shields.io/hackage/v/argon.svg?label=version&amp;style=flat-square">
    </a>
</p>

<p align="center">
    Argon measures your code's cyclomatic complexity.
</p>

<p align="center">
    <img alt="Argon screenshot"
         src="https://cloud.githubusercontent.com/assets/238549/10644166/5a0f5efc-7827-11e5-9b29-6e7bcccb2345.png">
</p>

<hr>

### Installing

Simple as ``cabal install argon``.

#### GHC compatibility

Argon parses Haskell using the standalone
[`ghc-lib-parser`](https://hackage.haskell.org/package/ghc-lib-parser), so it is
independent of the compiler used to build it. Version 0.4.2.0 is tested with
GHC 9.14 and understands the Haskell syntax of its pinned `ghc-lib-parser`
(currently the 9.14 series). Older `argon` releases targeting GHC 7.8–8.x can be
found on the [releases](https://github.com/rubik/argon/releases) page.

### About the complexity being measured

`argon` will compute the [cyclomatic
complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) of Haskell
functions, which is the number of decisions in a block of code plus 1. For
instance the following function:

```haskell
func n = case n of
           2 -> 3
           4 -> 6
           _ -> 42
```

has a cyclomatic complexity of 3.

The boolean operators `&&` and `||` also affect the this number. For instance
the following function:

```haskell
g n = n < 68 && n `mod` 3 == 2 && n > 49
```
has a cyclomatic complexity of 3.

As a last example, the following function:

```haskell
func n = case n of
           2 -> 3
           4 -> 6
           _ -> if 0 < n
                then 7
                else 8
```

has a cyclomatic complexity of 5.

Cyclomatic complexity provides a very shallow metric of code complexity: a high
cyclomatic complexity number does not necessarily mean that the function is
complex, and conversely, a low number does not necessarily indicate that the
function is simple. However, this number it can be useful for highlighting
potential maintainability issues.

### Running

The Argon executable expects a list of file paths (files or directories):

    $ argon --no-color --min 2 src
    src/Argon/Formatters.hs
        42:1 coloredFunc - 2
        43:11 color - 2
        57:1 formatResult - 2
    src/Argon/SYB/Utils.hs
        20:1 everythingStaged - 2
    src/Argon/Preprocess.hs
        34:1 toCpphsOptions - 2
        44:5 parseDefine - 2
    src/Argon/Loc.hs
        20:11 toRealSrcLoc - 2
    src/Argon/Walker.hs
        22:1 walk - 4
        15:1 allFiles - 2
    src/Argon/Results.hs
        46:1 filterNulls - 3
        66:1 exportStream - 3
        56:1 filterResults - 2
    src/Argon/Parser.hs
        67:1 parseModuleWithCpp - 3
        42:1 analyze - 2
        46:9 analysis - 2
        105:1 renderError - 2
    src/Argon/Cabal.hs
        23:11 toString - 3
    src/Argon/Types.hs
        81:5 toJSON - 2
    src/Argon/Visitor.hs
        61:1 visitExp - 6
        71:1 visitOp - 4
        30:11 visit - 2

For every file, Argon sorts results with the following criteria (and in this
order):

1. complexity (descending)
2. line number (ascending)
3. alphabetically

When colors are enabled (default), Argon computes a rank associated with the
complexity score:

| Complexity | Rank |
|:----------:|:----:|
|    0..5    |   A  |
|    5..10   |   B  |
|  above 10  |   C  |


#### JSON

Results can also be exported to JSON:
```json
$ argon --json --min 2 src
[
  {
    "blocks": [
      { "col": 1, "complexity": 2, "lineno": 42, "name": "coloredFunc" },
      { "col": 11, "complexity": 2, "lineno": 43, "name": "color" },
      { "col": 1, "complexity": 2, "lineno": 57, "name": "formatResult" }
    ],
    "path": "src/Argon/Formatters.hs",
    "type": "result"
  },
  {
    "blocks": [{ "col": 1, "complexity": 2, "lineno": 20, "name": "everythingStaged" }],
    "path": "src/Argon/SYB/Utils.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "col": 1, "complexity": 2, "lineno": 34, "name": "toCpphsOptions" },
      { "col": 5, "complexity": 2, "lineno": 44, "name": "parseDefine" }
    ],
    "path": "src/Argon/Preprocess.hs",
    "type": "result"
  },
  {
    "blocks": [{ "col": 11, "complexity": 2, "lineno": 20, "name": "toRealSrcLoc" }],
    "path": "src/Argon/Loc.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "col": 1, "complexity": 4, "lineno": 22, "name": "walk" },
      { "col": 1, "complexity": 2, "lineno": 15, "name": "allFiles" }
    ],
    "path": "src/Argon/Walker.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "col": 1, "complexity": 3, "lineno": 46, "name": "filterNulls" },
      { "col": 1, "complexity": 3, "lineno": 66, "name": "exportStream" },
      { "col": 1, "complexity": 2, "lineno": 56, "name": "filterResults" }
    ],
    "path": "src/Argon/Results.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "col": 1, "complexity": 3, "lineno": 67, "name": "parseModuleWithCpp" },
      { "col": 1, "complexity": 2, "lineno": 42, "name": "analyze" },
      { "col": 9, "complexity": 2, "lineno": 46, "name": "analysis" },
      { "col": 1, "complexity": 2, "lineno": 105, "name": "renderError" }
    ],
    "path": "src/Argon/Parser.hs",
    "type": "result"
  },
  {
    "blocks": [{ "col": 11, "complexity": 3, "lineno": 23, "name": "toString" }],
    "path": "src/Argon/Cabal.hs",
    "type": "result"
  },
  {
    "blocks": [{ "col": 5, "complexity": 2, "lineno": 81, "name": "toJSON" }],
    "path": "src/Argon/Types.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "col": 1, "complexity": 6, "lineno": 61, "name": "visitExp" },
      { "col": 1, "complexity": 4, "lineno": 71, "name": "visitOp" },
      { "col": 11, "complexity": 2, "lineno": 30, "name": "visit" }
    ],
    "path": "src/Argon/Visitor.hs",
    "type": "result"
  }
]
```
