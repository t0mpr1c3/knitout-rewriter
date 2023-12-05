#lang brag

;; https://doi.org/10.1145/3592449

pattern: statement*

statement: /SPACE* command? /SPACE* comment? /NEWLINE

command: TUCK /SPACE+ direction /SPACE+ needle /SPACE+ length /SPACE+ yarn
       | KNIT /SPACE+ direction /SPACE+ needle /SPACE+ length /SPACE+ yarn+
       | SPLIT /SPACE+ direction /SPACE+ needle /SPACE+ needle /SPACE+ length /SPACE+ yarn+
       | MISS /SPACE+ direction /SPACE+ needle /SPACE+ carrier
       | IN /SPACE+ direction /SPACE+ needle /SPACE+ carrier
       | OUT /SPACE+ direction /SPACE+ needle /SPACE+ carrier
       | DROP /SPACE+ needle
       | XFER /SPACE+ needle /SPACE+ needle
       | RACK /SPACE+ integer
       | NOP

comment: COMMENT

direction: DIR

needle: BED /DOT integer

length: positive-float

carrier: positive-integer

yarn : /LPAREN /SPACE* carrier /SPACE* /COMMA /SPACE* length /SPACE* /RPAREN /SPACE*

;; numeric

positive-integer: COUNT

integer: (INTEGER|COUNT)

positive-float: (POSITIVE-FLOAT|COUNT)

;; end