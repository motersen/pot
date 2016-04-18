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

(define base-path #f)

(define (init-base-path path)
	"Set the base directory for the database. Must be an existing directory."
	(if (directory-exists? path)
			(let ((path (path-strip-trailing-directory-separator
									 (path-normalize path))))
				(set! base-path path)
				base-path)
			(shout (string-append "Not an existing directory: " path))))

(define (get-base-path)
	base-path)

(define (get-db-path)
	(string-append (get-base-path) "/.pot"))

(define (request-db-dir)
	(let ((db-path (get-db-path)))
		(if (not (directory-exists? db-path))
				(with-exception-catcher
				 (lambda (x)
					 (let ((msg "Could not create database directory"))
						 (if (os-exception? x)
								 (shout (string-append msg ": "
																			 (err-code->string (os-exception-code x))))
								 (shout msg))))
				 (lambda () (create-directory db-path))))
		db-path))

(define (read-tag-index)
	(let ((index-path (string-append (get-db-path) "/.index")))
		(if (regular-file-exists? index-path)
				(call-with-input-file index-path
					(lambda (port)
						(read-all port read-line)))
				(list))))

(define (write-tag-index tags)
	(with-output-to-file
			(string-append (request-db-dir) "/.index")
		(lambda ()
			(for-each println tags))))

(define (add-tags-to-index tags)
	"Add sorted list of tags to the index."
	(write-tag-index (unite string<? (read-tag-index) tags)))

(define (remove-tags-from-index tags)
	"Remove sorted list of tags from index"
	(write-tag-index (differ string<? (read-tag-index) tags)))

(define (delete-tag-file tag)
	"Delete the file corresponding to tag."
	(let ((tag-file (string-append (get-db-path) "/" tag)))
		(if (regular-file-exists? tag-file)
				(delete-file tag-file)
				(yell (string-append "Tag '" tag "' does not exist.")))))

(define (read-resources-of-tag tag)
	(let ((register-path (string-append (get-db-path) "/" tag)))
		(if (regular-file-exists? register-path)
				(call-with-input-file register-path
					(lambda (port)
						(read-all port read-line)))
				(list))))

(define (write-resources-of-tag tag resources)
	(with-output-to-file
			(string-append (request-db-dir) "/" tag)
		(lambda ()
			(for-each println resources))))

(define (add-resources-to-tag tag resources)
	"Add sorted list of resources to tag."
	(let ((tag-resources (read-resources-of-tag tag)))
		(write-resources-of-tag tag (unite string<? tag-resources resources))
		;; return whether tag was empty before
		(null? tag-resources)))

(define (remove-resources-from-tag tag resources)
	"Remove sorted list of resources from tag."
	(let ((tag-resources (differ string<? (read-resources-of-tag tag) resources)))
		;; return whether tag is empty now
		(if (null? tag-resources)
				(or (delete-tag-file tag)
						#t)
				(and (write-resources-of-tag tag tag-resources)
						 #f))))

(define (tag tags resources)
	"Add resources to each of tags."
	(let ((resources (merge-sort string<? resources))
				(tags (merge-sort string<? tags)))
		(let tag ((tags tags) (new-tags '()))
			(if (null? tags)
					(if (pair? new-tags)
							;; new-tags was built in reverse order
							(add-tags-to-index (reverse new-tags)))
					(let ((new (add-resources-to-tag (car tags) resources)))
						(tag (cdr tags) (if new
																(cons (car tags) new-tags)
																new-tags)))))))

(define (untag tags resources)
	"Remove resources from each of tags."
	(let ((resources (merge-sort string<? resources))
				(tags (merge-sort string<? tags)))
		(let untag ((tags tags) (empty-tags '()))
			(if (null? tags)
					(if (pair? empty-tags)
							;; empty-tags was built in reverse order
							(remove-tags-from-index (reverse empty-tags)))
					(let ((empty (remove-resources-from-tag (car tags) resources)))
						(untag (cdr tags) (if empty
																	(cons (car tags) empty-tags)
																	empty-tags)))))))

(define (delete-tags tags)
	(let ((tags (merge-sort string<? tags)))
		(remove-tags-from-index tags)
		(map delete-tag-file tags)))
