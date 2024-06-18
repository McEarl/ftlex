# FLex

This is a library that provides an abstract ForTheL lexer that is independent
of concrete character encodings, position types, syntax highlighting
specifications etc. and can thus be used as a generic basis for a wide range of
ForTheL parsing frameworks.


## Usage

### Cabal

1.  * If you already have a file called `cabal.project` in your project directory,
      add `https://github.com/McEarl/FLex/releases/download/v0.1.4/flex-0.1.4.tar.gz`
      to `packages` there.

    * Otherwise, create a new file `cabal.project` that contains the following lines:

      ```cabal
      packages:
        ./*.cabal
        https://github.com/McEarl/FLex/releases/download/v0.1.4/flex-0.1.4.tar.gz
      ```

2.  Add `flex` to `build-depends` in your `<package name>.cabal`.


### Stack

1.  Add `https://github.com/McEarl/FLex/releases/download/v0.1.4/flex-0.1.4.tar.gz`
    to `extra-deps` in your `stack.yaml`.

2.  Add `flex` to `dependencies` in your `package.yaml`.
