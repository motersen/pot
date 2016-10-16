#!gsi-script -:d0

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

(init-base-path (current-directory))

(with-exception-catcher
 (exception-catcher "Sorry, something went wrong"
										all-exception-catchers)
 (lambda ()
	 (parse-options (lambda (command)
										(parse-command
										 command
										 filter-command
										 tag-command
										 untag-command
										 reverse-search-command
										 list-tags-command
										 delete-tags-command))
									(cdr (command-line))
									path-option
									version-option)))
