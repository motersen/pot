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

(define version "0.0.2")

(define (directory-exists? path)
	(and (file-exists? path)
			 (eq? 'directory (file-info-type (file-info path)))))

(define (regular-file-exists? path)
	(and (file-exists? path)
			 (eq? 'regular (file-info-type (file-info path)))))

(define raise-attention #f)
(define lower-attention #f)
(define current-attention #f)

(let ((attention 0))
	(set! raise-attention
		(lambda ()
			(if (>= attention 0)
					(set! attention (+ attention 1))
					(shout "-q and -l must not be used together."))))
	(set! lower-attention
		(lambda ()
			(if (<= attention 0)
					(set! attention (- attention 1))
					(shout "-q and -l must not be used together."))))
	(set! current-attention
		(lambda ()
			attention)))

(define (shout message)
	(println port: (current-error-port)
					 "Error: " message)
	(exit 1))

(define (yell message)
	(if (>= (current-attention) 0)
			(println port: (current-error-port)
							 "Warning: " message)))

(define (tell message)
	(if (>= (current-attention) 0)
			(println port: (current-error-port)
							 message)))

(define (mumble message)
	(if (>= (current-attention) 1)
			(println port: (current-error-port)
							 message)))

(define (whisper message)
	(if (>= (current-attention) 2)
			(println port: (current-error-port)
							 message)))

(define (print-version)
	(println "pot - version " version))

