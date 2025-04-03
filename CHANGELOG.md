# Changelog

## v0.5.0

* Provide two modes in which the TEX lexer can run:

  - `TexMode`: In this mode the lexer behaves like a proper TeX engine (modulo
    the parts of the TeX engine that are not yet implemented).

  - `FtlTexMode`: In this mode some adaptions are made to `TexMode` to imitate
    certain LaTeX features during lexing.
    (Currently only the initial category codes are adapted:
    `\NUL` becomes an invalid, `\DEL` an other, and `\t` a space character.)


## v0.4.0

* Allow only characters from the Unicode code blocks "Basic Latin" and
  "Latin-1 Supplement" (i.e. U+0000 to U+00FF) to occur in the input text.

* In the TEX lexer, horizontal tabs (U+0009) have now default category code
  10 (space).


## v0.3.9

* Revert the changes made in `v0.3.8` and handle backslashed line breaks
  properly: In the FTL lexer they do not get a special treatment while in the
  TEX lexer any escape character followed by a line break is turned into a
  control space


## v0.3.8

* Horizontal space characters preceeded by a backslash are not longer considered
  as line breaks.


## v0.3.7

* Add `Show` instances for lexemes.


## v0.3.6

* Move the position type from `Test.Main` to `FTLex.Position`.

* Globally disable warning for orphan instances.


## v0.3.5

* Add `Eq` and `Ord` instances for `Ftl.Lexeme p` and `Tex.Lexeme p`.

* Provide functions to check whether a lexeme is of a given type.


## v0.3.4

* Improved line break handling: Split the complete input text into lines that
  arethen processed by the lexer, where any of the strings `\r`, `\n` and `\r\n`
  is considered as a line break.

* Autmatically determine the ForTheL dialect from the file name extension of the
  input file in the test suite.


## v0.3.3

* Let the FTL and TEX lexers operate on `Text` again. (Since the lexers have to
  work with code points anyway, e.g. to determine their category codes,
  accepting `ByteString` instead of `Text` as input only procudes unnecessary
  overhead.)


## v0.3.2

* Fix positions of `^^`-escapes in the TEX lexer.


## v0.3.1

* Fix bugs concerning comments, positions and `^^`-escapes in the TEX lexer.


## v0.3.0

* Let the FTL and TEX lexers operate on (strict) `ByteString` instead of (lazy)
  `Text` input.

* Add the character encoding of the input text as a new argument to the lexer.
  The following are supported:

  - UTF-8
  - UTF-16 (little endian)
  - UTF-16 (big endian)
  - UTF-32 (little endian)
  - UTF-32 (big endian)

* Provide a test suite. See `README.md` for details.


## v0.2.2

* Provide debugging output.


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
