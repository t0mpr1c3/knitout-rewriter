#lang brag

;; https://doi.org/10.1145/3592449

pattern: statement*

statement: /SPACE* command? /SPACE* /SEMICOLON

command: TUCK /SPACE+ direction /SPACE+ needle /SPACE+ length /SPACE+ yarn
       | KNIT /SPACE+ direction /SPACE+ needle /SPACE+ length /SPACE+ yarn+
       | SPLIT /SPACE+ direction /SPACE+ needle /SPACE+ needle /SPACE+ length /SPACE+ yarn+
       | MISS /SPACE+ direction /SPACE+ needle /SPACE+ carrier
       | IN /SPACE+ direction /SPACE+ needle /SPACE+ carrier
       | OUT /SPACE+ direction /SPACE+ needle /SPACE+ carrier
       | DROP /SPACE+ needle
       | XFER /SPACE+ needle /SPACE+ needle
       | RACK /SPACE+ plus-or-minus-one
       | NOP

direction: DIR

needle: BED /DOT integer

length: positive-float

carrier: positive-integer

yarn : /LPAREN /SPACE* carrier /SPACE* /COMMA /SPACE* length /SPACE* /RPAREN /SPACE*

;; numeric

plus-or-minus-one: (ONE|MINUS-ONE)

positive-integer: (COUNT|ONE)

integer: (INTEGER|COUNT|ONE|MINUS-ONE)

positive-float: (POSITIVE-FLOAT|COUNT|ONE)

;; end