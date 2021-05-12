;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               example.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An example of hooking a lexer to com.informatimago.rdp parsers.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.RDP.EXAMPLE-WITH-LEXER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
  (:export "TEST/LEXER-SCANNER"
           "TEST/PARSE-EXAMPLE-NIL-LEXER"
           "TEST/EXAMPLE-NIL-LEXER"))
(in-package "COM.INFORMATIMAGO.RDP.EXAMPLE-WITH-LEXER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Language
;;; taken from: http://en.wikipedia.org/wiki/Recursive_descent_parser
;;;

(defgrammar example-nil-lexer
  :scanner nil
  :terminals ((ident "")
              (integer "")
              (real ""))
  :start program
  :rules ((--> factor
               (alt ident
                    number
                    (seq "(" expression ")" :action $2))
               :action $1)
          (--> number  (alt integer real) :action $1)
          (--> term
               factor (rep (alt "*" "/") factor)
               :action `(,$1 . ,$2))
          (--> expression
               (opt (alt "+" "-"))
               term
               (rep (alt "+" "-") term :action `(,$1 ,$2))
               :action `(+ ,(if $1 `(,$1 ,$2) $2)  . ,$3))
          (--> condition
               (alt (seq "odd" expression
                         :action `(oddp ,$2))
                    (seq expression
                         (alt "=" "#" "<" "<=" ">" ">=")
                         expression
                         :action `(,$2 ,$1 ,$3)))
               :action $1)
          (--> statement
               (opt (alt (seq ident ":=" expression
                              :action `(setf ,$1 ,$3))
                         (seq "call" ident
                              :action `(call ,$2))
                         (seq "begin" statement
                              (rep ";" statement
                                   :action $2)
                              "end"
                              :action `(,$2 . ,$3))
                         (seq "if" condition "then" statement
                              :action `(if ,$2 ,$4))
                         (seq "while" condition "do" statement
                              :action `(while ,$2 ,$4))))
               :action $1)
          (--> block
               (opt "const" ident "=" number
                    (rep "," ident "=" number
                         :action `(,$2 ,$4))
                    ";"
                    :action `((,$2 ,$4) . ,$5))
               (opt "var" ident
                    (rep "," ident :action $2)
                    ";"
                    :action `(,$2 . ,$3))
               (rep "procedure" ident ";" block ";"
                    :action `(procedure ,$2 ,$4))
               statement
               :action `(block ,$1 ,$2 ,$3 ,$4))
          (--> program
               block "." :action $1)))



#|

DEFGRAMMAR infers terminal tokens automatically, and let us specify
terminals by regexps (or pair of regexps), and is able to generate a
scanner from them.  eg.:

    (GRAMMAR-ALL-TERMINALS (GRAMMAR-NAMED 'example-nil-lexer))

But if you want to use another scanner it's possible to specify the
scanner you want with the :SCANNER option of DEFGRAMMAR:

SCANNER:

        Can be either:
        - T            A scanner is generated.
        - NIL          No scanner is generated.
        - a class-name subclass of COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER
                       which is used to get tokens.


Two options are possible: either :SCANNER NIL or :SCANNER some-scanner-class

On both cases, methods for the same generic functions need to be
provided.  With :SCANNER NIL, those methods are called on the source
object passed to the parser.  With :SCANNER some-scanner-class, a
scanner is instanciated with :SOURCE SOURCE.  In all cases, the parser
starts by calling: (ADVANCE-LINE SCANNER) to fetch the first token.

    (defgeneric scanner-end-of-source-p (scanner))
    (defgeneric advance-line            (scanner))
    (defgeneric accept                  (scanner token))
    (defgeneric scan-next-token (scanner &optional parser-data))
    (defgeneric scanner-source (scanner))
    (defgeneric scanner-file (scanner))
    (defgeneric scanner-line (scanner))
    (defgeneric scanner-column (scanner))
    (defgeneric scanner-state (scanner))
    (defgeneric scanner-spaces (scanner))
    (defgeneric scanner-tab-width (scanner))
    (defgeneric scanner-current-token (scanner))
    (defgeneric scanner-current-text (scanner))

It is expected that the scanner gives access to two tokens, the
current token, and the next token.  This is to support LALR(1)
parsers.  Therefore we will wrap the lexer into a lexer-scanner object
to keep track more easily of the required slots.

The RDP parser also needs methods for the tokens returned by the scanner:

    (token-kind token) --> symbol
    (token-text token) --> string

    (word-equal token symbol) --> generalized-boolean
    (word-equal token string) --> generalized-boolean
    (word-equal symbol token) --> generalized-boolean
    (word-equal string token) --> generalized-boolean

|#


(lexer:define-lexer example-lexer (state)
  
  ;; special characters: ( ) . % + - * ? [ ^ $
  ;; they can be escaped with %.

  ;; whitespaces include newlines and spaces:
  ("[
%s]+"  (values :next-token))
  
  ("<="        (values '<=        "<="))        
  (">="        (values '>=        ">="))        
  (":="        (values '\:=       ":="))

  ("%("        (values '\(        "("))         
  ("%)"        (values '\)        ")"))         
  ("%*"        (values '*         "*"))         
  ("/"         (values '/         "/"))         
  ("%+"        (values '+         "+"))         
  ("%-"        (values '-         "-"))         
  ("#"         (values '\#        "#"))         
  ("<"         (values '<         "<"))         
  (">"         (values '>         ">"))         
  ("="         (values '=         "="))         
  (","         (values '\,        ","))         
  (";"         (values '\;        ";"))         
  ("%."        (values '|.|       "."))

  ("procedure" (values 'procedure "procedure"))
  ("begin"     (values 'begin     "begin"))     
  ("const"     (values 'const     "const"))     
  ("while"     (values 'while     "while"))     
  ("call"      (values 'call      "call"))      
  ("then"      (values 'then      "then"))      
  ("end"       (values 'end       "end"))       
  ("odd"       (values 'odd       "odd"))       
  ("var"       (values 'var       "var"))       
  ("do"        (values 'do        "do"))        
  ("if"        (values 'if        "if"))        
           
  ;; real must come first to match the longest first.
  ("%d+%.%d+([Ee][-+]?%d+)?"     (values 'real    (read-from-string $$)))
  ("%d+"                         (values 'integer (parse-integer $$)))
  ("%a%w*"                       (values 'ident   $$)))



#|

The lexer-scanner could be a subclass of
COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER:BUFFERED-SCANNER or of
COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER:SCANNER, thus reusing
some features from those superclasses…  Or not.

|#

(defclass lexer-scanner ()
  ((lexer         :initarg :lexer :reader lexer)
   (line          :initarg :line   :initform 0 :accessor scanner-line)
   (column        :initarg :column :initform 0 :accessor scanner-column)
   (current-token :initform nil :accessor scanner-current-token)
   (next-token    :initform nil :accessor scanner-next-token)))

(defmethod initialize-instance :after ((scanner lexer-scanner) &key &allow-other-keys)
  (setf (scanner-next-token scanner) '%initial)
  (scan-next-token scanner))


(defmethod scanner-end-of-source-p ((scanner lexer-scanner))
  (when (null (scanner-current-token scanner))
    (setf (scanner-current-token scanner) (token-end-of-source-kind)))
  (eql (scanner-current-token scanner) (token-end-of-source-kind)))

(defmethod compute-scanner-column ((scanner lexer-scanner))
  (let* ((state  (lexer scanner))
         (buf    (lexer::lexstate-buf state))
         (text   (lexer:lexbuf-string buf))
         (line   (lexer:lexbuf-line buf)))
    (loop
      :for count :from (1- line) :downto 1
      :for eol := (position #\newline text)
        :then (position #\newline text :start (1+ eol))
      :finally (return (if eol
                           (- (lexer:lexbuf-pos buf) eol)
                           0)))))

(defmethod scan-next-token ((scanner lexer-scanner) &optional parser-data)
  (declare (ignore parser-data))
  (setf (scanner-line scanner) (lexer:lexbuf-line (lexer::lexstate-buf (lexer scanner)))
        (scanner-column scanner) (compute-scanner-column scanner))
  (shiftf (scanner-current-token scanner)
          (scanner-next-token scanner)
          (lexer:read-token (lexer scanner))))

(defmethod advance-line            ((scanner lexer-scanner))
  (if (and (not (eql (scanner-next-token scanner) '%initial))
           (scanner-end-of-source-p scanner))
      #|End of File -- don't move.|#
      (scanner-current-token scanner)
      (progn
        (scan-next-token scanner)
        (or (scanner-current-token scanner)
            ;; Just got EOF
            (setf (scanner-current-token scanner) (token-end-of-source-kind))))))

(defmethod accept                  ((scanner lexer-scanner) token)
  (unless (word-equal token (scanner-current-token scanner))
    (error "~D:~D: Unexpected token ~S; expected ~S"
           (scanner-line scanner)
           (scanner-column scanner)
           (scanner-current-token scanner)
           token))
  (prog1 (list (token-kind (scanner-current-token scanner))
               (scanner-current-text scanner)
               (scanner-column scanner))
    (scan-next-token scanner)))

(defmethod scanner-current-text ((scanner lexer-scanner))
  (if (eql (token-end-of-source-kind) (scanner-current-token scanner))
      "#<END-OF-SOURCE>"
      (lexer::token-lexeme (scanner-current-token scanner))))

(defmethod scanner-source ((scanner lexer-scanner))
  (lexer:lexbuf-string (lexer::lexstate-buf (lexer scanner))))

(defmethod scanner-file ((scanner lexer-scanner))
  (lexer:lexbuf-source (lexer::lexstate-buf (lexer scanner))))

(defmethod scanner-state ((scanner lexer-scanner))
  nil)


(defmethod token-kind ((token lexer::token)) (lexer::token-class token))
(defmethod token-text ((token lexer::token)) (lexer::token-lexeme token))

(defmethod word-equal ((token lexer::token) (kind symbol)) (eql   (token-kind token) kind))
(defmethod word-equal ((token lexer::token) (text string)) (equal (token-text token) text))
(defmethod word-equal ((kind symbol) (token lexer::token)) (eql   (token-kind token) kind))
(defmethod word-equal ((text string) (token lexer::token)) (equal (token-text token) text))






(defparameter *source* "
    const abc = 123,
          pi=3.141592e+0;
    var a,b,c;
    procedure gcd;
    begin
        while a # b do
        begin
             if a<b then b:=b-a ;
             if a>b then a:=a-b
        end
    end;
begin
    a:=42;
    b:=30.0;
    call gcd
end.")

(defun test/lexer-scanner ()
  (lexer:with-lexer (lexer 'example-lexer *source*)
    (let ((scanner (make-instance 'lexer-scanner :lexer lexer)))
      (advance-line scanner)
      (loop
        :for lin := (scanner-line scanner)
        :for col := (scanner-column scanner)
        :for tok := (scan-next-token scanner)
        :do (format t "~D:~D:~A~%" lin col tok)
        :until (scanner-end-of-source-p scanner)))))


(defun test/parse-example-nil-lexer ()
  (lexer:with-lexer (lexer 'example-lexer *source*)
    (let ((scanner (make-instance 'lexer-scanner :lexer lexer)))
      (parse-example-nil-lexer scanner))))


(define-test test/example-nil-lexer ()
  (check equal   (lexer:with-lexer (lexer 'example-lexer  "
    const abc = 123,
          pi=3.141592e+0;
    var a,b,c;
    procedure gcd;
    begin
        while a # b do
        begin
             if a<b then b:=b-a ;
             if a>b then a:=a-b
        end
    end;
begin
    a:=42;
    b:=30.0;
    call gcd
end.")
                   (let ((scanner (make-instance 'lexer-scanner :lexer lexer)))
                     (parse-example-nil-lexer scanner)))
                  
         '(block (((ident "abc" 14) (integer "123" 20))
                  ((ident "pi" 13) (real "3.141592e+0" 25)))
           ((ident "a" 10) (ident "b" 12) (ident "c" 14))
           ((procedure (ident "gcd" 18)
             (block nil
               nil
               nil
               ((((while ((\# "#" 18) (+ ((ident "a" 16))) (+ ((ident "b" 20))))
                    ((((if ((< "<" 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                           ((setf (ident "b" 27)
                                  (+ ((ident "b" 30)) ((- "-" 31) ((ident "a" 32))))))))
                      ((if ((> ">" 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                           ((setf (ident "a" 27)
                                  (+ ((ident "a" 30))
                                     ((- "-" 31) ((ident "b" 32)))))))))))))))))
           ((((setf (ident "a" 6) (+ ((integer "42" 10)))))
             ((setf (ident "b" 6) (+ ((real "30.0" 12))))) ((call (ident "gcd" 13))))))
         )
  :success)

#|
> (pprint (test/parse-example-nil-lexer))

(block (((ident "abc" 14) (integer "123" 20)) ((ident "pi" 13) (real "3.141592e+0" 25)))
  ((ident "a" 10) (ident "b" 12) (ident "c" 14))
  ((procedure (ident "gcd" 18)
    (block nil
      nil
      nil
      ((((while ((\# "#" 18) (+ ((ident "a" 16))) (+ ((ident "b" 20))))
          ((((if ((< "<" 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                 ((setf (ident "b" 27) (+ ((ident "b" 30)) ((- "-" 31) ((ident "a" 32))))))))
            ((if ((> ">" 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                 ((setf (ident "a" 27) (+ ((ident "a" 30)) ((- "-" 31) ((ident "b" 32)))))))))))))))))
  ((((setf (ident "a" 6) (+ ((integer "42" 10))))) ((setf (ident "b" 6) (+ ((real "30.0" 12)))))
    ((call (ident "gcd" 13))))))


> (test/example-nil-lexer)
(test/example-nil-lexer)                    1 test,    1 success.
:success

|#
;;;; THE END ;;;;
