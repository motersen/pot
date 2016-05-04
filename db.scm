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

(define (find-resource-in-tag-register-data v xs)
	(define (find-beginning-of-line v i)
		(if (= 0 i)
				0
				(if (= (char->integer #\newline) (u8vector-ref v (- i 1)))
						i
						(find-beginning-of-line v (- i 1)))))
	(define (find-end-of-line v i)
		(if (= (char->integer #\newline) (u8vector-ref v i))
				i
				(find-end-of-line v (+ i 1))))
	(define (compare xs v i)
		(if (null? xs)
				(if (= (char->integer #\newline) (u8vector-ref v i))
						0
						-1)
				(if (< (car xs) (u8vector-ref v i))
						-1
						(if (> (car xs) (u8vector-ref v i))
								1
								(compare (cdr xs) v (+ i 1))))))
	(let ((left-bound 0)
				(right-bound (- (u8vector-length v) 1)))
		(let* ((cont (call/cc (lambda (cc) cc)))
					 (offset (+ left-bound (floor (/ (- right-bound left-bound) 2))))
					 (beginning-of-line (find-beginning-of-line v offset))
					 (comparison (compare xs v beginning-of-line)))
			(if (= comparison 0)
					beginning-of-line
					(begin
						(if (= comparison -1)
								(set! right-bound (- (find-beginning-of-line v offset) 1))
								(set! left-bound (+ (find-end-of-line v offset) 1)))
						(if (> left-bound right-bound)
								#f
								(cont cont)))))))

(define (find-tags-of-resource resource)
	(let ((unicode-resource-name (map char->integer (string->list resource))))
		(let find ((tags (read-tag-index)))
				(if (null? tags)
					(list)
					(if (find-resource-in-tag-register-data
							 (read-entire-file (string-append (get-db-path) "/" (car tags)))
							 unicode-resource-name)
							(cons (car tags) (find (cdr tags)))
							(find (cdr tags)))))))
