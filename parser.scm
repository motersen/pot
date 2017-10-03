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

(define (tag-char? char)
	(define (char-ascii-alpha? char)
		(let ((codepoint (char->integer char)))
			(or (<= #x41 codepoint #x5a)			; uppercase ascii letter
					(<= #x61 codepoint #x7a))))		; lowercase ascii letter
	(define (char-ascii-digit? char)
		(<= #x30 (char->integer char) #x39))
	(or (char-ascii-alpha? char)
			(char-ascii-digit? char)
			(char=? #\- char)
			(char=? #\: char)))

(define (if-nth xs n)
	(and (pair? xs)
			 (if (= 0 n)
					 (car xs)
					 (if-nth (cdr xs) (- n 1)))))

(define (tokenize op? s)
	(let tokenize ((s (string->list s))
								 (tokens (list)))
		(if (null? s)
				(reverse tokens)
				(let ((c (car s)))
					(cond
					 ((tag-char? c)
						(let ((tag.rest (collect-tag s)))
							(tokenize (cdr tag.rest) (cons (car tag.rest) tokens))))
					 ((op? c)
						(tokenize (cdr s) (cons (list->string (list c)) tokens)))
					 ((char=? #\( c)
						(let ((group.rest (tokenize (cdr s) (list))))
							(tokenize (cdr group.rest) (cons (car group.rest) tokens))))
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
			 (let ((resources (read-resources-of-tag (car tokens))))
	 (if (null? resources)
			 (mumble (string-append "Tag '" (car tokens) "' does not exist.")))
	 resources)))

(define (parse-filter tokens)
	(or (parse-union tokens)
			(parse-difference tokens)
			(parse-intersection tokens)
			(parse-tag tokens)
			(parse-parens tokens)
			(syntax-fail tokens)))

(define (parse-tag-list str)
	(define (expected token)
		(shout (string-append "Ill-formed tag-list "
													"'" str "'"
													": Expected "
													token)))
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
