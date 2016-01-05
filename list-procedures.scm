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

(define (unite predicate a b)
	"merge two lists sorted by predicate into one such list"
	"remove duplicates across lists"
	(if (or (null? a) (null? b))
			(append a b)
			(let ((ca (car a)) (cb (car b)))
				(if (predicate ca cb)
						(cons ca (unite predicate (cdr a) b))
						(if (predicate cb ca)
								(cons cb (unite predicate a (cdr b)))
								(cons cb (unite predicate (cdr a) (cdr b))))))))

(define (differ predicate a b)
	"a without b"
	(if (or (null? a) (null? b))
			a
			(let ((ca (car a)) (cb (car b)))
				(if (predicate ca cb)
						(cons ca (differ predicate (cdr a) b))
						(if (predicate cb ca)
								(differ predicate a (cdr b))
								(differ predicate (cdr a) (cdr b)))))))

(define (intersect predicate a b)
	"Intersection of a and b"
	(if (or (null? a) (null? b))
			(list)
			(let ((ca (car a)) (cb (car b)))
				(if (predicate ca cb)
						(intersect predicate (cdr a) b)
						(if (predicate cb ca)
								(intersect predicate a (cdr b))
								(cons ca (intersect predicate (cdr a) (cdr b))))))))

(define (alternating-bisect xs)
	(let bisect ((xs xs) (sections '(())))
		(if (not (pair? xs))
				(cons (reverse (car sections))
							(reverse (cdr sections)))
				(bisect (cdr xs) (cons (cdr sections)
															 (cons (car xs) (car sections)))))))

(define (merge-sort predicate xs)
	(and (pair? xs)
			 (if (null? (cdr xs))
					 xs
					 (let ((halves (alternating-bisect xs)))
						 (unite predicate
										(merge-sort predicate (car halves))
										(merge-sort predicate (cdr halves)))))))
