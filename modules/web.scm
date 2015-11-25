;; ##################################################################
;;
;; MassMine: Your Access To Big Data
;; Copyright (C) 2014-2015  Nicholas M. Van Horn & Aaron Beveridge
;; Author: Nicholas M. Van Horn
;; 
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Instructions: See www.massmine.org for complete documentation and
;; instructions for how to use massmine

(module massmine-web *

  (import scheme chicken)
  (use extras data-structures srfi-1 srfi-13)
  (use rest-bind uri-common medea http-client html-parser)

  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "0.10.0 (2015-10-09)" #f)))

  ;; Available tasks and brief descriptions
  (define web-task-descriptions
    '((web-text . "Extract all text from a given web page (url)")))

  ;; Command line arguments supported by each task
  (define wikipedia-task-options
    '((web-text . "query* (one or more URLs separated by spaces)")))

  ;; Core url fetcher. Content retrieved from url is returned as a
  ;; string (plus two other http-client values)
  (define (url-content->string url)
    (handle-exceptions exn
	(begin
	  (display "\nProblem fetching url: " (current-error-port))
	  (display ((condition-property-accessor 'exn 'message) exn)
		   (current-error-port))
	  (newline (current-error-port))
	  (exit 1))
      (with-input-from-request url #f read-string)))

  ;; Full text url scraper. Packages and returns the url, text, and
  ;; timestamp in JSON. Multiple urls can be passed in a single
  ;; string, separated by whitespace. e.g.,
  ;; "http://www.first.com http://www.first.com".
  ;; Outputs to current-output-port
  (define (web-text url-string)
    (for-each
     (lambda (curr-url)
       (let ((raw-text (with-input-from-string (url-content->string curr-url)
			 (lambda () (html-strip)))))
	 (write-json `((url . ,curr-url)
		       (timestamp . ,(time->string (seconds->local-time
						    (current-seconds))))
		       (text . ,raw-text)))
	 (newline)))
     (string-split url-string)))

  ) ;; end of module massmine-web

;; web.scm ends here
