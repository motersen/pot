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
;; the list of arguments to parse. The parser should express the list of
;; remaining arguments

(define (parse-command args . parsers)
	"Parse a command and its arguments."
	(if (not (pair? args))
			(shout "No command given."))
	(let ((cont (call/cc (lambda (cc) (cc cc)))))
		(if (null? parsers)
				(shout (string-append "Invalid command: " (car args))))
		(let ((p (car parsers)))
			(set! parsers (cdr parsers))
			(p (lambda () (cont cont)) args))))

(define (parse-flags next-arg flags args parsers)
	(let next-flag ((flags flags)
									(args args))
		(let cont ((parsers parsers)
							 (rest-flags flags)
							 (rest-args args))
			(if (null? rest-flags)
					(next-arg rest-args)
					(if (null? parsers)
							(if (eqv? rest-flags flags)
									(shout (string-append "Unknown option: -"
																				(car rest-flags)))
									(next-flag rest-flags rest-args))
							((caar parsers)
							 (lambda (flags args)
								 (cont (cdr parsers) flags args))
							 rest-flags
							 rest-args))))))

(define (parse-long-option next-arg opt args parsers)
	(let cont ((opt opt)
						 (args args)
						 (parsers parsers))
		(if (null? opt)
				(next-arg args)
				(if (null? parsers)
						(shout (string-append "Unknown Option: --" opt))
						((cdar parsers)
						 (lambda (opt args)
							 (cont opt args (cdr parsers)))
						 opt
						 args)))))

(define (parse-options parse-command args . parsers)
	"Parse recognized options at front of args and express remaining arguments"
	(let parse-options ((args args))
		(if (not (pair? args))
				(parse-command (list))
				(let ((arg (string->list (car args))))
					(if (not (eq? (car arg) #\-))
							(parse-command args)
							(if (not (pair? (cdr arg)))
									(parse-options (cdr args))
									(if (not (eq? (cadr arg) #\-))
											(parse-flags parse-options
																	 (map (lambda (c) (string c))
																				(cdr arg))
																	 (cdr args)
																	 parsers)
											(if (not (pair? (cddr arg)))
													(parse-options (cdr args))
													(parse-long-option parse-options
																						 (list->string (cddr arg))
																						 (cdr args)
																						 parsers)))))))))

(define-macro (make-option-parser flag long-option success)
	`(cons
		(lambda (cont flags args)
			(if (not (string=? ,flag (car flags)))
					(cont flags args)
					(,success
					 (lambda (args)
						 (cont (cdr flags) args))
					 args)))
		(lambda (cont option args)
			(if (not (string=? ,long-option option))
					(cont option args)
					(,success
					 (lambda (args)
						 (cont (list) args))
					 args)))))

(define path-option
	(make-option-parser
	 "p" "path"
	 (lambda (cont args)
		 (if (not (pair? args))
				 (shout "No argument to --path option."))
		 (init-base-path (car args))
		 (cont (cdr args)))))

(define louder-option
	(make-option-parser
	 "l" "louder"
	 (lambda (cont args)
		 (raise-attention)
		 (cont args))))

(define quieter-option
	(make-option-parser
	 "q" "quieter"
	 (lambda (cont args)
		 (lower-attention)
		 (cont args))))

(define version-option
	(make-option-parser
	 "v" "version"
	 (lambda (cont args)
		 (print-version))))

(define (filter-command cont args)
	(if (not (member? string=? (car args) '("f" "filter")))
			(cont)
			(begin
				(if (not (pair? (cdr args)))
						(shout "No filter given."))
				(if (pair? (cddr args))
						(yell "Too many arguments to filter command."))
				(for-each println
									(parse-filter-string (cadr args))))))

(define (list-tags-command cont args)
	(if (not (member? string=? (car args) '("lt" "list-tags")))
			(cont)
			(begin
				(if (pair? (cdr args))
						(yell "Too many arguments to list-tags command."))
				(for-each println (read-tag-index)))))

(define (tag-command cont args)
	(if (not (member? string=? (car args) '("t" "tag")))
			(cont)
			(if (not (pair? (cdr args)))
					(shout "No Tag-List given.")
					(tag (parse-tag-list (cadr args))
							 (if (pair? (cddr args))
									 (cddr args)
									 (read-all (current-input-port) read-line))))))

(define (untag-command cont args)
	(if (not (member? string=? (car args) '("u" "untag")))
			(cont)
			(if (not (pair? (cdr args)))
					(shout "No Tag-List given.")
					(untag (parse-tag-list (cadr args))
								 (if (pair? (cddr args))
										 (cddr args)
										 (read-all (current-input-port) read-line))))))

(define (delete-tags-command cont args)
	(if (not (member? string=? (car args) '("d" "delete-tags")))
			(cont)
			(delete-tags (fold (lambda (xs xt) (unite string<? xs xt)) '()
												 (map (lambda (str)
																(merge-sort string<? (parse-tag-list str)))
															(if (pair? (cdr args))
																	(cdr args)
																	(read-all (current-input-port) read-line)))))))

(define (reverse-search-command cont args)
	(if (not (member? string=? (car args) '("r" "reverse-search")))
			(cont)
			(if (not (pair? (cdr args)))
					(shout "No resource given.")
					(for-each println (find-tags-of-resource (cadr args))))))
