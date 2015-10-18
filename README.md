## Argon

Argon measures cyclomatic complexity in your Haskell code.

### Installing

Simple as ``stack install`` or ``cabal install``.

### Running

The Argon executable expects a list of file paths:

    $ argon src/**/*.hs
    src/Argon/Parser.hs
        13:1 filenameMode - A (1)
        19:1 parseCode - A (1)
    src/Argon/Pretty.hs
        30:1 formatResult - A (2)
        14:1 fore - A (1)
        20:1 coloredFunc - A (1)
        23:1 coloredRank - A (1)
        33:11 single - A (1)
    src/Argon/Visitor.hs
        35:1 visitExp - B (8)
        15:1 funCC - A (3)
        17:11 name - A (2)
        12:1 funcsCC - A (1)
        21:1 sumWith - A (1)
        24:1 complexity - A (1)
        29:11 descend - A (1)
