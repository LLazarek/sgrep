#lang at-exp rscript

(require utill/read-module
         syntax/parse)

(define (sgrep-file path pattern [recursive? #f])
  (define mod-stx (with-handlers ([exn:fail? (const #'())])
                    (read-module path)))
  (define mod-sexp (syntax->datum mod-stx))
  (sgrep-sexp mod-sexp pattern recursive?))

(define ns (make-base-namespace))
(eval '(require racket racket/match (for-syntax syntax/parse)) ns)

(require (for-syntax syntax/parse)
         racket/match)
#;(define-match-expander list-no-order*
  (syntax-parser
    [(_ head no-order-pats ...)
     #'(cons head (list-no-order no-order-pats ...))]))

(eval (syntax->datum
       #'(define-match-expander list-no-order*
          (syntax-parser
            [(_ head no-order-pats (... ...))
             #'(cons head (list-no-order no-order-pats (... ...)))])))
      ns)
(eval (syntax->datum
       #'(define-match-expander list-containing
          (syntax-parser
            [(_ pat)
             #'(list-no-order pat _ ___)])))
      ns)

(module+ test
  (require ruinit)
  (test-begin
    (test-equal? (eval '(match '(head 1 2 3)
                          [(list-no-order* 'head 2 _ ___) 'yes]
                          [else 'no])
                       ns)
                 'yes)))

(define (sgrep-sexp s pattern [recursive? #f])
  (let loop ([s s])
    (cond [(try-match s pattern)
           (list s)]
          [recursive?
           (match s
             [(list sub-exps ...)
              (append* (map loop sub-exps))]
             [datum empty])]
          [else empty])))

(define (try-match s pattern)
  (eval `(match ',s
           [,pattern #t]
           [else #f])
        ns))

(define-logger sgrep)

(main
 #:arguments ([(hash-table ['recursive? recursive?]) (list* pattern-str paths)]
              #:once-each
              [("-r" "--recursive")
               'recursive?
               ("Recursively search module syntax for the pattern."
                "The default is to just match the module syntax as a whole against the pattern."
                "Warning: this can be slow!")
               #:record]
              #:args [match-pattern . paths])
 (define pattern (call-with-input-string pattern-str read))
 (define paths-to-search
   (match paths
     ['() (list (current-directory))]
     [specific-files-or-dirs specific-files-or-dirs]))
 (define-values {files-to-search dirs-to-search}
   (partition path-to-existant-file? paths-to-search))
 (for ([f (apply in-sequences
           (in-list files-to-search)
           (map in-directory dirs-to-search))]
       #:when (path-has-extension? f ".rkt"))
   (log-sgrep-info (~a @(system/string "date +'%r'") f))
   (define results (sgrep-file f pattern recursive?))
   (unless (empty? results)
     (displayln f)
     (pretty-write results)
     (newline))))
