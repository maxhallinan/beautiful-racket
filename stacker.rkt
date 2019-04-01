#lang br/quicklang

; The reader and the expander for a language are often defined in different files.

; Questions I have after doing this:
; - What is handle conceptually? Handle seems just as essential as reader and expander
;   but its meaning is swept under the rug.
;   It seems like handle is used as a runtime/evaluator.
; - What is all this about `HANDLE-EXPR ...`? 
;   I have no idea what this does or what it's about.
; - What is all of this about syntax objects, modules, and datums? 
;   This information seems essential but it isn't introduced. 
;   Again, crucial information is swept under the rug.

; Define the reader.
; A reader takes a path to the file and a port to the file.
; The port is an interface for reading the contents of the file.
; The reader returns a module. 
; A module is a structured representation of a source file.
; The module points to the expander.
; read-syntax : String -> Port -> Module
(define (read-syntax path port)
  ; Read the entire content of the file into memory
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  ; Defines a module named `stacker-mod` that uses the expander in `stacker.rkt` 
  ; to evaluate the src-datums expression.
  (define module-datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  ; datum->syntax converts a datum to a syntax object.
  ; The first argument is program context to bind to the second argument when it is evaluated.
  ; `#f` means that no context is bound.
  (datum->syntax #f module-datum))
(provide read-syntax)

; Define the expander.
(define-macro (stacker-module-begin HANDLE-EXPR ...) 
              ; `#'` creates a syntax object that captures the current lexical context.
              #'(#%module-begin 
                 HANDLE-EXPR ...
                 (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!) 
  (define x (first stack)) 
  (set! stack (rest stack)) 
  x)

(define (push-stack! x) 
  (set! stack (cons x stack)))

; handle takes one argument with a default value of #f
(define (handle [x #f]) 
  ; cond expressions have two branches
  (cond 
    ; first branch: if x is a number, push it onto the stack
    [(number? x) (push-stack! x)]
    ; second branch: check if x is one of the operators
    ; `cond` continues evaluating the expressions until it finds one that evaluates to false
    ; if `(or (equal? + x) (equal? * x)) is false, then cond will not evaluate the rest of the expressions
    [(or (equal? + x) (equal? * x))
     (define op-result (x (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))
(provide handle)

(provide + *)
