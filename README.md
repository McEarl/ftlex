# FTLex – Formula Theory Lexer

This is a library that provides an abstract ForTheL lexer that is independent
of concrete character encodings, position types, syntax highlighting
specifications etc. and can thus be used as a generic basis for a wide range of
ForTheL parsing frameworks.


## Usage

### Cabal

1.  * If you already have a file called `cabal.project` in your project directory,
      add `https://github.com/McEarl/ftlex/archive/refs/tags/v0.3.4.tar.gz`
      to `packages` there.

    * Otherwise, create a new file `cabal.project` that contains the following lines:

      ```cabal
      packages:
        ./*.cabal
        https://github.com/McEarl/ftlex/archive/refs/tags/v0.3.4.tar.gz
      ```

2.  Add `ftlex` to `build-depends` in your `<package name>.cabal`.


### Stack

1.  Add `https://github.com/McEarl/ftlex/archive/refs/tags/v0.3.4.tar.gz`
    to `extra-deps` in your `stack.yaml`.

2.  Add `ftlex` to `dependencies` in your `package.yaml`.


## Supported Formats of Input Texts

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


## Development

### Running the Test Suites

To run the test suites of this package, execute the following commands:

```
cabal configure --enable-tests
cabal build
cabal test --test-options="<path to ForTheL file>" --test-show-details=direct
```

(Note: The option `--test-show-details=direct` is necessary to allow user
interaction during a test run.)


### Generating a Source Distribution File

To generate a tarball of this package – which will be located at
`dist-newstyle/sdist/ftlex-0.3.4.tar.gz` – simply execute the following command:

```
cabal sdist
```
