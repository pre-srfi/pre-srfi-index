;; Scrape pre-SRFIs from a file listing of
;; https://github.com/johnwcowan/r7rs-work

(import (scheme base) (scheme write) (srfi 1) (srfi 13))

(cond-expand
  (gauche (import (only (gauche base) sys-basename))
          (import (only (file util) path-sans-extension))))

(define (filename-stem filename)
  (path-sans-extension (sys-basename filename)))

(define (displayln x) (display x) (newline))

(define (read-all-lines)
  (let loop ((lines '()))
    (let ((line (read-line)))
      (if (eof-object? line) (reverse lines) (loop (cons line lines))))))

;;

(define base-url "https://github.com/johnwcowan/r7rs-work/blob/master/")

(define people
  '(
    ("Cowan")
    ("Curtis")
    ("Durusau" "PatrickDurusau")
    ("Ganz" "SteveGanz" "StevenGanz")
    ("Gleckler" "ArthurGleckler")
    ("Gloria")
    ("Harvey" "BrianHarvey")
    ("Hemann")
    ("Lucier" "BradleyLucier")
    ("Medernach" "EmmanuelMedernach")
    ("Radul" "AlexeyRadul")
    ("Read" "JeffreyRead")
    ("Rush" "DavidRush")
    ("Russell" "BenjaminRussell")
    ("Shinn" "AlexShinn")
    ("Shivers" "OlinShivers")
    ("SnellPym" "AlaricSnellPym")
    ("Sussman" "GeraldSussman")

    ("Arcfide" "AaronHsu")
    ("Riastradh")

    ("Alexandria")
    ("Gauche")
    ("MIT")
    ("MitTeam")
    ))

(define person-last-name car)
(define person-full-names cdr)

(define ideas
  '(
    "BottomScheme"
    "CL-R"
    "ExpressionLanguage"
    "FatArch"
    "DivisionUseCases"
    "ImplementationsMay"
    "ImplementationsShould"
    ))

(define surveys
  '(
    "ApplyArgsLimit"
    "ArgumentOrder"
    "BackslashBar"
    "HashBangEof"
    "BracketsBraces"
    "CallCc"
    "CaseInsensitivity"
    "CaseSensitivity"
    "ChezReplSemantics"
    "CaseSensitivityArcfide"
    "CompilerAvailable"
    "ComplexLog"
    "ComplexRepresentations"
    "CondExpand"
    "CondExpandShinn"
    "EmptyList"
    "EmptyStringsVectors"
    "ExceptionTaxonomies"
    "FixnumInfo"
    "FloatPrecision"
    "ImplementationContrasts"
    "ImplementationSupport"
    "DotComma"
    "SchemeOnWindows"
    "SelfQuotingVectors"
    "SixRejection"
    ))

(define (editorial? stem)
  (any (lambda (candidate) (string-contains stem candidate))
       '("Docket" "Edition" "Draft" "WG1" "WG2")))

(define (personal-page? stem)
  (any (lambda (person) (member stem (person-full-names person)))
       people))

(define (relevant? stem)
  (not (or (personal-page? stem)
           (member stem surveys)
           (member stem ideas)
           (editorial? stem))))

(define (split-author-names stem)
  (let loop ((names '()) (stem stem))
    (let ((name (any (lambda (person)
                       (let ((last-name (person-last-name person)))
                         (and (string-suffix? last-name stem)
                              last-name)))
                     people)))
      (if (not name)
          (cons stem (reverse names))
          (loop (cons name names)
                (substring stem 0 (- (string-length stem)
                                     (string-length name))))))))

(define (main)
  (for-each displayln
            (map split-author-names
                 (filter relevant?
                         (map filename-stem (read-all-lines))))))

(main)
