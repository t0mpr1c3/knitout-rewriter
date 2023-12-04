#lang brag

;; https://textiles-lab.github.io/knitout/knitout.html

pattern: line*

line: /SPACE* command? /SPACE* comment? /NEWLINE

comment: COMMENT

command: IN /SPACE+ carriers
       | INHOOK /SPACE+ carriers
       | RELEASEHOOK /SPACE+ carriers
       | OUT /SPACE+ carriers
       | OUTHOOK /SPACE+ carriers
       | STITCH /SPACE+ size /SPACE+ size
       | RACK /SPACE+ (INTEGER|COUNT)
       | KNIT /SPACE+ direction /SPACE+ needle /SPACE+ carriers
       | TUCK /SPACE+ direction /SPACE+ needle /SPACE+ carriers
       | SPLIT /SPACE+ direction /SPACE+ needle /SPACE+ needle /SPACE+ carriers
       | DROP /SPACE+ needle
       | AMISS /SPACE+ needle
       | XFER /SPACE+ needle /SPACE+ needle
       | MISS /SPACE+ direction /SPACE+ needle /SPACE+ carriers
       | PAUSE

direction: DIR

needle: BED (INTEGER|COUNT)

size: (FLOAT|INTEGER|COUNT)

;; carriers must be integers > 0, not arbitrary strings as in Knitout spec
carriers: COUNT (/SPACE+ COUNT)*

;; end