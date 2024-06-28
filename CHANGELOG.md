# Changelog


## v0.2.1

* Rename *FLex* to *FTLex* to avoid confusion with
  [Flex](https://github.com/westes/flex).


## v0.2.0

* Allow the default set of allowed characters in the input text (Basic Latin)
  to be extended by the following Unicode blocks:

  - Latin-1 Supplement
  - Latin Extended-A
  - Latin Extended-B
  - IPA Extensions

* Ignore a BOM at the beginning of an input text.

* Merge the module `Flex.Split` into `Flex.Base`.


## v0.1.4

* Relax version boundaries.

* Add usage instructions for Cabal.

* Fix line break handling in TeX lexer.


## v0.1.3

* Move splitter to own module.

* Export initial lexing state.


## v0.1.2

* Save the text a lexeme is generated from.

* Remove EOF lexeme.

* Remove line break category.


## v0.1.1

* Replace `String` by `Text`.

* Remove labels.

* Rename functions in `Flex.Position`.


## v0.1.0

Change lexing style to TeX-like line-by-line lexing.


## v0.0.5

Revise `runLexer` signatures.


## v0.0.4

Revise module structure.


## v0.0.3

* Expose `Flex.CatCode`, `Flex.Message` and `Flex.Position`.

* Fix error message.

* Fix default character codes.


## v0.0.2

* Less restrictive version bounds for build dependencies.

* Add usage instructions for Stack.


## v0.0.1

Initial version.
