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

(define (parse-options parse-command args . parsers)
	"Parse all options and pass the remaining arguments to the command parser."
	(if (null? args)
			(parse-command args)
			(let parse-options ((args args))
				(let ((parsers parsers))
					(let* ((cont #f)
								 (rest-args (call/cc (lambda (cc) (set! cont cc) args))))
						(if (null? parsers)
								(if (eqv? args rest-args)
										(parse-command args)
										(parse-options rest-args))
								(let ((p (car parsers)))
									(set! parsers (cdr parsers))
									(p cont rest-args))))))))

(define (path-option cont args)
	(if (not (member? string=? (car args) '("-p" "-path")))
			(cont args)
			(begin
				(if (not (pair? (cdr args)))
						(shout "No argument to path option."))
				(init-base-path (cadr args))
				(cont (cddr args)))))

(define (version-option cont args)
	(if (not (member? string=? (car args) '("-v" "-version")))
			(cont args)
			(print-version)))

(define (filter-command cont args)
	(if (not (member? string=? (car args) '("f" "filter")))
			(cont)
			(begin
				(if (not (pair? (cdr args)))
						(shout "No filter given."))
				(if (pair? (cddr args))
						(yell "Too many arguments to filter command."))
				(for-each println
									(parse-filter
									 (tokenize
										(op-check ",;/")
										(cadr args)))))))

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
					(tag (parse-tag-list (cadr args)) (cddr args)))))

(define (untag-command cont args)
	(if (not (member? string=? (car args) '("u" "untag")))
			(cont)
			(if (not (pair? (cdr args)))
					(shout "No Tag-List given.")
					(untag (parse-tag-list (cadr args)) (cddr args)))))

(define (delete-tags-command cont args)
	(if (not (member? string=? (car args) '("d" "delete-tags")))
			(cont)
			(begin
				(if (not (pair? (cdr args)))
						(shout "No Tag-List given."))
				(if (pair? (cddr args))
						(yell "Too many arguments to delete-tags command"))
				(delete-tags (parse-tag-list (cadr args))))))
