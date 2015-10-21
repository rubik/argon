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
         src="https://cloud.githubusercontent.com/assets/238549/10630521/5a60346c-77d7-11e5-8e87-373bec72e777.png">
</p>

<hr>

### Installing

Simple as ``stack install argon`` or ``cabal install argon``.

### Running

The Argon executable expects a list of file paths:

    $ argon --no-color src/**/*.hs
    src/Argon/Formatters.hs
        47:1 formatResult - 3
        33:1 coloredFunc - 2
        27:1 fore - 1
        37:1 coloredRank - 1
    src/Argon/Parser.hs
        51:1 parseCode - 2
        44:1 handleExc - 1
    src/Argon/Results.hs
        17:1 filterResults - 2
        22:1 export - 2
    src/Argon/Visitor.hs
        35:1 visitExp - 5
        42:1 visitOp - 4
        14:1 funCC - 3
        16:11 name - 2
        11:1 funcsCC - 1
        20:1 sumWith - 1
        23:1 complexity - 1
        28:11 descend - 1
        32:11 inspect - 1

For every file, Argon sorts results with the following criteria (and in this
order):

    * complexity (descending)
    * line number (ascending)
    * alphabetically

When colors are enabled (default), Argon computes a rank associated with the
coomplexity score:

| Complexity | Rank |
|:----------:|:----:|
|    0..5    |   A  |
|    5..10   |   B  |
|  above 10  |   C  |


#### JSON

Optionally, results can also be exported to JSON:

    $ argon --json src/**/*.hs
    [
      {
        "blocks": [
          { "complexity": 3, "name": "formatResult", "lineno": 58, "col": 1 },
          { "complexity": 2, "name": "coloredFunc", "lineno": 39, "col": 1 },
          { "complexity": 1, "name": "fore", "lineno": 33, "col": 1 },
          { "complexity": 1, "name": "coloredRank", "lineno": 43, "col": 1 },
          { "complexity": 1, "name": "formatSingle", "lineno": 52, "col": 1 }
        ],
        "path": "src/Argon/Formatters.hs",
        "type": "result"
      },
      { "blocks": [ ], "path": "src/Argon.hs", "type": "result" },
      {
        "blocks": [
          { "complexity": 2, "name": "parseCode", "lineno": 55, "col": 1 },
          { "complexity": 1, "name": "handleExc", "lineno": 48, "col": 1 }
        ],
        "path": "src/Argon/Parser.hs",
        "type": "result"
      },
      {
        "blocks": [
          { "complexity": 3, "name": "export", "lineno": 35, "col": 1 },
          { "complexity": 2, "name": "filterResults", "lineno": 28, "col": 1 },
          { "complexity": 1, "name": "sortOn", "lineno": 12, "col": 1 }
        ],
        "path": "src/Argon/Results.hs",
        "type": "result"
      },
      {
        "blocks": [
          { "complexity": 2, "name": "toJSON", "lineno": 30, "col": 5 },
          { "complexity": 1, "name": "toJSON", "lineno": 18, "col": 5 }
        ],
        "path": "src/Argon/Types.hs",
        "type": "result"
      },
      {
        "blocks": [
          { "complexity": 5, "name": "visitExp", "lineno": 36, "col": 1 },
          { "complexity": 4, "name": "visitOp", "lineno": 43, "col": 1 },
          { "complexity": 3, "name": "funCC", "lineno": 15, "col": 1 },
          { "complexity": 2, "name": "name", "lineno": 17, "col": 11 },
          { "complexity": 1, "name": "funcsCC", "lineno": 12, "col": 1 },
          { "complexity": 1, "name": "sumWith", "lineno": 21, "col": 1 },
          { "complexity": 1, "name": "complexity", "lineno": 24, "col": 1 },
          { "complexity": 1, "name": "descend", "lineno": 29, "col": 11 },
          { "complexity": 1, "name": "inspect", "lineno": 33, "col": 11 }
        ],
        "path": "src/Argon/Visitor.hs",
        "type": "result"
      }
    ]
