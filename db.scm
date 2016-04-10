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
	"Set the relative root of tagged files. Must be an existing directory."
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

(define (read-files-of-tag tag)
	(let ((register-path (string-append (get-db-path) "/" tag)))
		(if (regular-file-exists? register-path)
				(call-with-input-file register-path
					(lambda (port)
						(read-all port read-line)))
				(begin
					(yell (string-append "Tag '" tag "' does not exist."))
					(list)))))

(define (write-files-of-tag tag files)
	(with-output-to-file
			(string-append (request-db-dir) "/" tag)
		(lambda ()
			(for-each println files))))

(define (add-files-to-tag tag files)
	"Add sorted list of files to tag."
	(let ((tag-files (read-files-of-tag tag)))
		(write-files-of-tag tag (unite string<? tag-files files))
		;; return whether tag was empty before
		(null? tag-files)))

(define (remove-files-from-tag tag files)
	"Remove sorted list of files from tag."
	(let ((tag-files (differ string<? (read-files-of-tag tag) files)))
		;; return whether tag is empty now
		(if (null? tag-files)
				(or (delete-tag-file tag)
						#t)
				(and (write-files-of-tag tag tag-files)
						 #f))))

(define (tag tags files)
	"Add files to each of tags."
	(let ((files (merge-sort string<? files))
				(tags (merge-sort string<? tags)))
		(let tag ((tags tags) (new-tags '()))
			(if (null? tags)
					(if (pair? new-tags)
							;; new-tags was built in reverse order
							(add-tags-to-index (reverse new-tags)))
					(let ((new (add-files-to-tag (car tags) files)))
						(tag (cdr tags) (if new
																(cons (car tags) new-tags)
																new-tags)))))))

(define (untag tags files)
	"Remove files from each of tags."
	(let ((files (merge-sort string<? files))
				(tags (merge-sort string<? tags)))
		(let untag ((tags tags) (empty-tags '()))
			(if (null? tags)
					(if (pair? empty-tags)
							;; empty-tags was built in reverse order
							(remove-tags-from-index (reverse empty-tags)))
					(let ((empty (remove-files-from-tag (car tags) files)))
						(untag (cdr tags) (if empty
																	(cons (car tags) empty-tags)
																	empty-tags)))))))

(define (delete-tags tags)
	(let ((tags (merge-sort string<? tags)))
		(remove-tags-from-index tags)
		(map delete-tag-file tags)))
