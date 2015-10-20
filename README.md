## Argon

Argon measures cyclomatic complexity in your Haskell code.

### Installing

Simple as ``stack install`` or ``cabal install``.

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
