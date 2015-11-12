<h1 align="center">
    <a href="https://github.com/rubik/argon">
        Argon
    </a>
</h1>

<p align="center">
    <a href="https://travis-ci.org/rubik/argon">
        <img alt="Tests"
             src="https://img.shields.io/travis/rubik/argon.svg?style=flat-square">
    </a>
    <a href="https://coveralls.io/github/rubik/argon">
        <img alt="Code coverage"
             src="https://img.shields.io/coveralls/rubik/argon.svg?style=flat-square">
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

Simple as ``stack install argon`` or ``cabal install argon``.
Note: if you are using Stack and your resolver if too old, you might have to
add some packages to your `stack.yaml` file.

#### GHC compatibility

Argon is compatible with GHC 7.8 and 7.10, it's tested against those versions
only.

### Running

The Argon executable expects a list of file paths (files or directories):

    $ argon --no-color --min 2 src
    src/Argon/Types.hs
        61:5 toJSON - 2
    src/Argon/Visitor.hs
        55:1 visitExp - 5
        62:1 visitOp - 4
        28:11 visit - 2
        35:1 getFuncName - 2
    src/Argon/Parser.hs
        55:1 parseModuleWithCpp - 3
        88:1 customLogAction - 3
        35:1 analyze - 2
        39:9 analysis - 2
    src/Argon/Formatters.hs
        61:1 formatResult - 3
        42:1 coloredFunc - 2
        43:11 color - 2
    src/Argon/Results.hs
        35:1 export - 3
        28:1 filterResults - 2
    src/Argon/Loc.hs
        18:11 toRealSrcLoc - 2

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
  { "blocks": [ ], "path": "src/Argon.hs", "type": "result" },
  {
    "blocks": [{ "complexity": 2, "name": "toJSON", "lineno": 61, "col": 5 }],
    "path": "src/Argon/Types.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "complexity": 5, "name": "visitExp", "lineno": 55, "col": 1 },
      { "complexity": 4, "name": "visitOp", "lineno": 62, "col": 1 },
      { "complexity": 2, "name": "visit", "lineno": 28, "col": 11 },
      { "complexity": 2, "name": "getFuncName", "lineno": 35, "col": 1 }
    ],
    "path": "src/Argon/Visitor.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "complexity": 3, "name": "parseModuleWithCpp", "lineno": 55, "col": 1 },
      { "complexity": 3, "name": "customLogAction", "lineno": 88, "col": 1 },
      { "complexity": 2, "name": "analyze", "lineno": 35, "col": 1 },
      { "complexity": 2, "name": "analysis", "lineno": 39, "col": 9 }
    ],
    "path": "src/Argon/Parser.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "complexity": 3, "name": "formatResult", "lineno": 61, "col": 1 },
      { "complexity": 2, "name": "coloredFunc", "lineno": 42, "col": 1 },
      { "complexity": 2, "name": "color", "lineno": 43, "col": 11 }
    ],
    "path": "src/Argon/Formatters.hs",
    "type": "result"
  },
  {
    "blocks": [
      { "complexity": 3, "name": "export", "lineno": 35, "col": 1 },
      { "complexity": 2, "name": "filterResults", "lineno": 28, "col": 1 }
    ],
    "path": "src/Argon/Results.hs",
    "type": "result"
  },
  {
    "blocks": [{ "complexity": 2, "name": "toRealSrcLoc", "lineno": 18, "col": 11 }],
    "path": "src/Argon/Loc.hs",
    "type": "result"
  },
  { "blocks": [ ], "path": "src/Argon/Preprocess.hs", "type": "result" }
]
```
