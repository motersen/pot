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

;srfi-1 procedures required by pot

(define (any l xs)
	(and (pair? xs)
			 (or (l (car xs))
					 (any l (cdr xs)))))

(define (member? predicate x xs)
	(and (pair? xs)
			 (if (equal? x (car xs))
					 xs
					 (member? predicate x (cdr xs)))))

(define (fold kons knil xs)
	(if (not (pair? xs))
			knil
			(fold kons (kons (car xs) knil) (cdr xs))))
