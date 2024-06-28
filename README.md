# FTLex – Formula Theory Lexer

This is a library that provides an abstract ForTheL lexer that is independent
of concrete character encodings, position types, syntax highlighting
specifications etc. and can thus be used as a generic basis for a wide range of
ForTheL parsing frameworks.


## Usage

### Cabal

1.  * If you already have a file called `cabal.project` in your project directory,
      add `https://github.com/McEarl/FTLex/releases/download/v0.2.0/ftlex-0.2.0.tar.gz`
      to `packages` there.

    * Otherwise, create a new file `cabal.project` that contains the following lines:

      ```cabal
      packages:
        ./*.cabal
        https://github.com/McEarl/FTLex/releases/download/v0.2.0/ftlex-0.2.0.tar.gz
      ```

2.  Add `ftlex` to `build-depends` in your `<package name>.cabal`.


### Stack

1.  Add `https://github.com/McEarl/FTLex/releases/download/v0.2.0/ftlex-0.2.0.tar.gz`
    to `extra-deps` in your `stack.yaml`.

2.  Add `ftlex` to `dependencies` in your `package.yaml`.


## Expected Format of Input Texts

* The input text is expected to be encoded in UTF-8.

* A leading BOM is permitted, but not necessary.

* The following types of line breaks are supported:

  - CR, i.e. `\r`
  - LF, i.e. `\n`
  - CRLF, i.e. `\r\n`

* Characters from the following Unicode blocks are supported:

  - Basic Latin
  - Latin-1 Supplement
  - Latin Extended-A
  - Latin Extended-B
  - IPA Extensions
