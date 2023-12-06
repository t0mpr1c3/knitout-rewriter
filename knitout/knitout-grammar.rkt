#lang brag

;; https://textiles-lab.github.io/knitout/knitout.html#lang brag

;; https://doi.org/10.1145/3592449

knitout: magic-number script

magic-number: VERSION /NEWLINE

script: header* pattern

header: HEADER /NEWLINE

pattern: statement*

statement: /SPACE* command? /SPACE* comment? /NEWLINE

comment: (HEADER|COMMENT)

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

carriers: (COUNT|CARRIER) (/SPACE+ (COUNT|CARRIER))*

;; end