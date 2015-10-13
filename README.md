
Haskell bindings to the primecount C++ library:

https://github.com/kimwalisch/primecount

Note: This is an experimental package, and the API /
module names are subject to change.

To Build
===

1. First install the [primecount][1] library.

2. Run `stack build`

This version is designed to wrok with version 2.3 of
the primecount library (commit [3ef01b][2].)

If you get an error like:

    Configuring hprimecount-0.1.0.0...
    Setup.hs: Missing dependency on a foreign library:
    * Missing C library: primecount

then use the `--extra-lib-dirs ...` option to tell stack
where to find the library:

    stack build --extra-lib-dirs <dir>

[1]: https://github.com/kimwalisch/primecount 
[2]: https://github.com/kimwalisch/primecount/commit/3ef01bba198d6c0f05c776c9f72cea278c6cee88

