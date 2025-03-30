#lang racket

;; A recursive-descent parser for the given grammar

;; Define a structure for tokens
(struct token (type value line) #:transparent)

;; Define a structure for the parser state
(struct parser-state (tokens position) #:mutable #:transparent)

;; Lexical analysis function
(define (tokenize input-filename)
  (define line-num 1)
  
  ;; Nested helper functions for lexical analysis
  (define (next-line)
    (set! line-num (add1 line-num)))
  
  (define (get-id port)
    (let loop ([chars '()])
      (let ([c (peek-char port)])
        (if (and (not (eof-object? c)) (or (char-alphabetic? c) (char-numeric? c)))
            (begin
              (read-char port)
              (loop (cons c chars)))
            (reverse chars)))))
  
  (define (tokenize-port port)
    ;; Nested helper for collecting digits when parsing numbers
    (define (collect-digits str)
      (let ([next-c (peek-char port)])
        (if (and (not (eof-object? next-c)) (char-numeric? next-c))
            (begin
              (read-char port)
              (collect-digits (string-append str (string next-c))))
            str)))
    
    (let loop ([tokens '()])
      (let ([c (peek-char port)])
        (cond
          ;; End of file
          [(eof-object? c) 
           (reverse (cons (token 'EOF "$$" line-num) tokens))]
          
          ;; Whitespace
          [(char-whitespace? c)
           (read-char port)
           (when (char=? c #\newline) (next-line))
           (loop tokens)]
          
          ;; Identifiers
          [(char-alphabetic? c)
           (let ([id (list->string (get-id port))])
             (cond 
               [(string=? id "if") (loop (cons (token 'IF id line-num) tokens))]
               [(string=? id "endif") (loop (cons (token 'ENDIF id line-num) tokens))]
               [(string=? id "read") (loop (cons (token 'READ id line-num) tokens))]
               [(string=? id "write") (loop (cons (token 'WRITE id line-num) tokens))]
               [else (loop (cons (token 'ID id line-num) tokens))]))]
          
          ;; Numbers
          [(or (char-numeric? c) (char=? c #\+) (char=? c #\-))
           (if (and (or (char=? c #\+) (char=? c #\-))
                    (let ([next (peek-char port 1)])
                      (and (not (eof-object? next)) (not (char-numeric? next)))))
               ;; It's a + or - operator, not a number sign
               (let ([op (string (read-char port))])
                 (loop (cons (token 'COMP op line-num) tokens)))
               ;; It's part of a number
               (let* ([sign-char (if (or (char=? c #\+) (char=? c #\-)) 
                                    (string c) 
                                    "")]
                      [_ (when (not (string=? sign-char "")) (read-char port))]
                      [digits-str (collect-digits "")]
                      [num-str (string-append sign-char digits-str)])
                 (if (string=? digits-str "")
                     ;; Handle the case where there are no digits after a sign
                     (begin
                       (when (not (string=? sign-char ""))
                         ;; Put the sign back as a comparison operator
                         (loop (cons (token 'COMP sign-char line-num) tokens)))
                       (loop tokens))
                     (loop (cons (token 'NUM num-str line-num) tokens)))))]
          
          ;; Special characters
          [(char=? c #\=)
           (read-char port)
           (if (and (not (eof-object? (peek-char port))) (char=? (peek-char port) #\=))
               (begin
                 (read-char port)
                 (loop (cons (token 'COMP "==" line-num) tokens)))
               (loop (cons (token 'EQUAL "=" line-num) tokens)))]
          
          [(char=? c #\%)
           (read-char port)
           (loop (cons (token 'COMP "%" line-num) tokens))]

          [(char=? c #\*)
           (read-char port)
           (loop (cons (token 'COMP "*" line-num) tokens))]

          [(char=? c #\/)
           (read-char port)
           (loop (cons (token 'COMP "/" line-num) tokens))]
          
          [(char=? c #\<)
           (read-char port)
           (if (and (not (eof-object? (peek-char port))) (char=? (peek-char port) #\=))
               (begin
                 (read-char port)
                 (loop (cons (token 'COMP "<=" line-num) tokens)))
               (loop (cons (token 'COMP "<" line-num) tokens)))]
          
          [(char=? c #\>)
           (read-char port)
           (if (and (not (eof-object? (peek-char port))) (char=? (peek-char port) #\=))
               (begin
                 (read-char port)
                 (loop (cons (token 'COMP ">=" line-num) tokens)))
               (loop (cons (token 'COMP ">" line-num) tokens)))]
          
          [(char=? c #\!)
           (read-char port)
           (if (and (not (eof-object? (peek-char port))) (char=? (peek-char port) #\=))
               (begin
                 (read-char port)
                 (loop (cons (token 'COMP "!=" line-num) tokens)))
               (error (format "Unexpected character '!' at line ~a" line-num)))]
          
          [(char=? c #\;)
           (read-char port)
           (loop (cons (token 'SEMI ";" line-num) tokens))]
          
          [(char=? c #\()
           (read-char port)
           (loop (cons (token 'LPAREN "(" line-num) tokens))]
          
          [(char=? c #\))
           (read-char port)
           (loop (cons (token 'RPAREN ")" line-num) tokens))]
          
          [(and (char=? c #\$) (char=? (peek-char port 1) #\$))
           (read-char port) (read-char port)
           (loop (cons (token 'EOF "$$" line-num) tokens))]
          
          [else (error (format "Unexpected character '~a' at line ~a" c line-num))]))))
  
  ;; Run the tokenizer on the input file
  (call-with-input-file input-filename
    (lambda (port)
      (tokenize-port port))))

;; Main parse function
(define (parse filename)
  ;; Nested helper functions for the parser
  (define (current-token state)
    (if (< (parser-state-position state) (length (parser-state-tokens state)))
        (list-ref (parser-state-tokens state) (parser-state-position state))
        (token 'EOF "$$" 0)))

  (define (consume! state)
    (set-parser-state-position! state (add1 (parser-state-position state))))

  (define (match? state type)
    (eq? (token-type (current-token state)) type))
  
  (define (parse-error state message)
    (let ([token (current-token state)])
      (error (format "Syntax error at line ~a: ~a (near '~a')" 
                     (token-line token) message (token-value token)))))
  
  ;; Recursive descent parsing functions (all nested within parse)
  (define (parse-program state)
    (let ([stmt-list (parse-stmt-list state)])
      (if (match? state 'EOF)
          (consume! state)
          (parse-error state "Expected end of file"))
      (list 'program stmt-list)))

  (define (parse-stmt-list state)
    (if (or (match? state 'EOF) (match? state 'ENDIF))
        '() ; epsilon case
        (let ([stmt (parse-stmt state)])
          (cons stmt (parse-stmt-list state)))))

  (define (parse-stmt state)
    (cond
      ;; Assignment statement: id = expr;
      [(match? state 'ID)
       (let* ([id-tree (parse-id state)])
         (if (match? state 'EQUAL)
             (begin
               (consume! state)
               (let ([expr (parse-expr state)])
                 (if (match? state 'SEMI)
                     (begin
                       (consume! state)
                       (list 'assign id-tree expr))
                     (parse-error state "Expected ';'"))))
             (parse-error state "Expected '='")))]
      
      ;; If statement: if (expr) stmt_list endif;
      [(match? state 'IF)
       (consume! state)
       (if (match? state 'LPAREN)
           (begin
             (consume! state)
             (let ([expr (parse-expr state)])
               (if (match? state 'RPAREN)
                   (begin
                     (consume! state)
                     (let ([stmt-list (parse-stmt-list state)])
                       (if (match? state 'ENDIF)
                           (begin
                             (consume! state)
                             (if (match? state 'SEMI)
                                 (begin
                                   (consume! state)
                                   (list 'if-stmt expr stmt-list))
                                 (parse-error state "Expected ';'")))
                           (parse-error state "Expected 'endif'"))))
                   (parse-error state "Expected ')'"))))
           (parse-error state "Expected '('"))]
      
      ;; Read statement: read id;
      [(match? state 'READ)
       (consume! state)
       (if (match? state 'ID)
           (let ([id-tree (parse-id state)])
             (if (match? state 'SEMI)
                 (begin
                   (consume! state)
                   (list 'read id-tree))
                 (parse-error state "Expected ';'")))
           (parse-error state "Expected identifier"))]
      
      ;; Write statement: write expr;
      [(match? state 'WRITE)
       (consume! state)
       (let ([expr (parse-expr state)])
         (if (match? state 'SEMI)
             (begin
               (consume! state)
               (list 'write expr))
             (parse-error state "Expected ';'")))]
      
      [else (parse-error state "Expected statement")]))

  (define (parse-expr state)
    (cond
      ;; Expression starting with ID
      [(match? state 'ID)
       (let ([id-tree (parse-id state)])
         (let ([etail (parse-etail state)])
           (if (null? etail)
               (list 'expr id-tree)
               (list 'expr id-tree etail))))]
      
      ;; Expression starting with NUM
      [(match? state 'NUM)
       (let ([num-tree (parse-num state)])
         (let ([etail (parse-etail state)])
           (if (null? etail)
               (list 'expr num-tree)
               (list 'expr num-tree etail))))]
      
      [else (parse-error state "Expected expression")]))
  
  (define (parse-num state)
    (define (parse-numsign num-str)
      (cond
        [(string=? num-str "") "epsilon"]
        [(char=? (string-ref num-str 0) #\+) "+"]
        [(char=? (string-ref num-str 0) #\-) "-"]
        [else "epsilon"]))
    
    (define (parse-digits digits-str)
      (if (string=? digits-str "")
          '() ; handle empty string case
          (let loop ([i 0] [result '()])
            (if (< i (string-length digits-str))
                (loop (add1 i) 
                      (append result 
                              (list (list 'digit (string (string-ref digits-str i))))))
                result))))
    
    (when (not (match? state 'NUM))
      (parse-error state "Expected number"))
    
    (let* ([num-str (token-value (current-token state))]
           [numsign (parse-numsign num-str)]
           ;; Extract the digit portion after the sign (if any)
           [digit-str (if (or (string=? numsign "+") (string=? numsign "-"))
                        (substring num-str 1)
                        num-str)]
           ;; Ensure there's at least one digit
           [digits (if (> (string-length digit-str) 0)
                       (parse-digits digit-str)
                       (parse-error state "Expected at least one digit after sign"))])
      (consume! state)
      (list 'num numsign digits)))

  (define (parse-id state)
    (when (not (match? state 'ID))
      (parse-error state "Expected identifier"))
    
    (let ([id-str (token-value (current-token state))])
      (consume! state)
      (list 'id id-str)))

  (define (parse-etail state)
    (cond
      ;; + expr
      [(and (match? state 'COMP) (string=? (token-value (current-token state)) "+"))
       (consume! state)
       (let ([expr (parse-expr state)])
         (list '+ expr))]
      
      ;; - expr
      [(and (match? state 'COMP) (string=? (token-value (current-token state)) "-"))
       (consume! state)
       (let ([expr (parse-expr state)])
         (list '- expr))]
      
      ;; compare expr
      [(match? state 'COMP)
       (let ([comp (token-value (current-token state))])
         (consume! state)
         (let ([expr (parse-expr state)])
           (list comp expr)))]
      
      ;; epsilon
      [else '()]))

  ;; Main parsing logic
  (with-handlers 
      ([exn:fail? 
        (lambda (exn)
          (format "Error: ~a" (exn-message exn)))])
    
    (let* ([tokens (tokenize filename)]
           [state (parser-state tokens 0)]
           [parse-tree (parse-program state)])
      (format "Accept\n~a" parse-tree))))

;; Main function to run the parser with user input
(define (main)
  (display "Enter the filename to parse: ")
  (flush-output)
  (let ([filename (read-line)])
    (if (file-exists? filename)
        (display (parse filename))
        (display (format "Error: File '~a' not found.\n" filename)))))

;; Run main when the file is executed directly
(module+ main
  (main))

;; Export the parse function
(provide parse)