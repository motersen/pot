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

;; Each element of parsers must be a function of one argument which is
;; the list of arguments to parse. The parser should return the list of
;; remaining arguments
(define (parse-args args . parsers)
	"Parse a list of arguments using selected parsers."
	(let parse-args ((args args))
		(if (not (pair? args))
				(list)
				(let try ((parsers parsers))
					;; no parse succeeded
					(if (null? parsers)
							args
							(let ((rest ((car parsers) args)))
								;; parse not successful
								(if (eqv? args rest)
										;; try other parsers
										(try (cdr parsers))
										;; success, parse other arguments
										(parse-args rest))))))))

(define (parse-command args . parsers)
	"Parse a command and its arguments using selected parsers."
	(if (not (pair? args))
			(shout "No command given.")
			(let parse-command ((args args) (parsers parsers))
				(if (pair? args)
						(if (pair? parsers)
								(parse-command ((car parsers) args) (cdr parsers))
								(shout (string-append "Invalid command: " (car args))))))))

(define (path-option args)
	(if (not (member? string=? (car args) '("-p" "-path")))
			args
			(if (not (pair? (cdr args)))
					(shout "No argument to path option.")
					(begin
						(init-base-path (cadr args))
						(cddr args)))))

(define (lower-attention-option args)
	(let ((option (string->list (car args))))
		(if (not (and (char=? #\- (car option))
									(char=? #\q (cadr option))))
				args
				;; support -quiet
				(let process ((flags (cddr option)))
					(lower-attention)
					(if (not (pair? flags))
							(cdr args)
							(if (not (char=? #\q (car flags)))
									(shout "Illegal flag in -q option")
									(process (cdr flags))))))))

(define (filter-command args)
	(if (not (member? string=? (car args) '("f" "filter")))
			args
			(if (not (pair? (cdr args)))
					(shout "No filter given.")
					(let ((filter (cadr args))
								(rest (cddr args)))
						(if (pair? rest)
								(yell "Too many arguments to filter command."))
						(for-each println
											(parse-filter
											 (tokenize
												(op-check ",;/")
												(cadr args))))
						(list)))))

(define (list-tags-command args)
	(if (not (member? string=? (car args) '("lt" "list-tags")))
			args
			(begin
				(if (pair? (cdr args))
						(yell "Too many arguments to list-tags command."))
				(for-each println (read-tag-index))
				(list))))

(define (tag-command args)
	(if (not (member? string=? (car args) '("t" "tag")))
			args
			(if (not (pair? (cdr args)))
					(shout "No Tagnames given.")
					(let ((tag-list (cadr args))
								(files (cddr args)))
						(tag (parse-tag-list tag-list)
								 files)
						(list)))))

(define (untag-command args)
		(if (not (member? string=? (car args) '("u" "untag")))
			args
			(if (not (pair? (cdr args)))
					(shout "No Tagnames given.")
					(let ((tag-list (cadr args))
								(files (cddr args)))
						(untag (parse-tag-list tag-list)
									 files)
						(list)))))

