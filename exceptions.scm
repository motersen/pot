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

(define (exception-catcher msg handlers)
	"Universal exception catching engine"
	(lambda (x)
		(let ((cont (call/cc (lambda (cc) (cc cc)))))
			(if (null? handlers)
					(shout msg)
					(let ((h (car handlers)))
						(set! handlers (cdr handlers))
						(h (lambda () (cont cont)) x msg))))))

(define (os-exception-catcher cont x msg)
	(if (os-exception? x)
			(shout (string-append msg
														(let ((os-msg (os-exception-message x)))
															(if os-msg
																	(string-append ": " os-msg)
																	""))
														" - "
														(err-code->string (os-exception-code x))))
			(cont)))

(define (no-such-f-o-d-exception-catcher cont x msg)
	(if (no-such-file-or-directory-exception? x)
			(shout (string-append msg ": " "No such file or directory"))
			(cont)))

(define all-exception-catchers (list os-exception-catcher
																		 no-such-f-o-d-exception-catcher))
