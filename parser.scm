;;;   Copyright 2016 Moritz Petersen
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.

; requires: srfi-1

(define (op-check operators)
	(let ((op-chars (string->list operators)))
		(lambda (c) (member? char=? c op-chars))))

(define tag-char?
	(let ((tag-chars (string->list (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
																								"abcdefghijklmnopqrstuvwxyz"
																								"0123456789-"))))
		(lambda (c)
			(member? char=? c tag-chars))))

(define (if-nth xs n)
	(and (pair? xs)
			 (if (= 0 n)
					 (car xs)
					 (if-nth (cdr xs) (- n 1)))))

(define (tokenize op? s)
	(let tokenize ((s (string->list s))
								 (tokens (list)))
		(if (null? s)
				tokens
				(let ((c (car s)))
					(cond
					 ((tag-char? c)
						(let ((tag-rest (collect-tag s)))
							(tokenize (cdr tag-rest) (append tokens (list (car tag-rest))))))
					 ((op? c)
						(tokenize (cdr s)
											(append tokens (list (list->string (list c))))))
					 ((char=? #\( c)
						(let ((rest (tokenize (cdr s) '())))
							(tokenize (cdr rest) (append tokens (list (car rest))))))
					 ((char=? #\) c)
						(cons tokens (cdr s)))
					 (#t
						(shout (string-append
										"Not a valid symbol: '"
										(list->string (list c))
										"'"))))))))

(define (collect-tag symbols)
	"pair of tag-string formed by first symbols and rest of symbols or #f"
	(and (pair? symbols)
			 (tag-char? (car symbols))
			 (let collect-tag ((tag (list (car symbols)))
												 (rest (cdr symbols)))
				 (if (and (pair? rest)
									(tag-char? (car rest)))
						 (collect-tag (cons (car rest) tag) (cdr rest))
						 (cons (list->string (reverse tag)) rest)))))

(define (syntax-fail tokens)
	(shout #\' tokens #\' "Is not a correct sentence."))

(define-macro (parse-combination combinator operator)
	`(lambda (tokens)
		 (let find-op ((a '()) (b tokens))
			 (and (not (null? b))
						(if (and (string? (car b)) (string=? (car b) ,operator))
								(,combinator string<? (parse-filter a) (parse-filter (cdr b)))
								(find-op (append a (list (car b))) (cdr b)))))))

(define parse-union (parse-combination unite ";"))
(define parse-difference (parse-combination differ "/"))
(define parse-intersection (parse-combination intersect ","))

(define (parse-parens tokens)
	(and (pair? tokens) (pair? (car tokens))
			 (parse-filter (car tokens))))

(define (parse-tag tokens)
	(and (pair? tokens)
			 (string? (car tokens))
			 (read-files-of-tag (car tokens))))

(define (parse-filter tokens)
	(or (parse-union tokens)
			(parse-difference tokens)
			(parse-intersection tokens)
			(parse-tag tokens)
			(parse-parens tokens)
			(syntax-fail tokens)))

(define (parse-tag-list str)
	(define (expected str)
		(shout (string-append "Ill-formed tag-list: Expected "
													str)))
	(let parse-tag-list ((symbols (string->list str)))
		(if (null? symbols)
				(list)
				(let ((tag-rest (collect-tag symbols)))
					(if (not tag-rest)
							(expected "Tagname")
							(if (not (pair? (cdr tag-rest)))
									(list (car tag-rest))
									(if (not (eq? #\, (cadr tag-rest)))
											(expected ",")
											(cons (car tag-rest)
														(parse-tag-list (cddr tag-rest))))))))))
