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
;; x.x.x 2015-05-01 NVH: Initial chicken version
;;
;; Instructions: See www.massmine.org for complete documentation and
;; instructions for how to use massmine

(module massmine-twitter *

  (import scheme chicken)
  (use extras irregex data-structures posix utils)
  (use openssl oauth-client uri-common rest-bind medea clucker)

  ;; This returns the user's twitter credentials, if available. If the
  ;; user has not provided this information previously, an error is
  ;; reported 
  (define (twitter-auth params)
    (let ((twitter-cred-file (string-append (alist-ref 'mm-cred-path params) "/"
					    "twitter_cred")))
      (if (file-exists? twitter-cred-file)
	  ;; Return twitter credential information
	  (with-input-from-file twitter-cred-file read)
	  ;; Else the user needs set up their credentials
	  (begin
	    (display "Authenticate before using Twitter.\nRun --> 'massmine --task=twitter-auth'\n"
		     (current-error-port))
	    (exit 1)))))

  ;; This walks the user through setting up their Twitter credentials
  ;; for MassMine
  (define (twitter-setup-auth params)
    (print "Would you like to setup your Twitter credentials?")
    (print "Warning: continuing will over-write any previous credentials")
    (if (yes-or-no? "Continue?" #:default "No" #:abort #f)
	;; Walk the user through setting up their credentials
	(let ((twitter-cred-file (string-append
				  (alist-ref 'mm-cred-path params)
				  "/"
				  "twitter_cred")))
	  (print "Please visit https://apps.twitter.com to collect")
	  (print "the following information:")
	  (display "Consumer key: ")
	  (define c-key (read-line))
	  (display "Consumer secret: ")
	  (define c-secret (read-line))
	  (display "Access token: ")
	  (define a-token (read-line))
	  (display "Access token secret: ")
	  (define a-secret (read-line))
	  ;; Prepare a proper alist and write to disk
	  (with-output-to-file twitter-cred-file
	    (lambda ()
	      (write `((consumer-key . ,c-key)
		(consumer-secret . ,c-secret)
		(access-token . ,a-token)
		(access-token-secret . ,a-secret)))))
	  (print "\nAuthentication setup finished!"))
	(print "Stopping!")))

  (define (twitter-stream pattern geo-locations lang-code)
    (handle-exceptions exn
  	;; Exception handler does nothing but suppress the inevitable
  	;; error caused but terminating the connection manually
  	#t
      ;; This does the real work
      (statuses-filter-method #:delimited "length"
			      #:track pattern
			      #:locations geo-locations
			      #:language lang-code)))

  ;; Task for retrieving list of locations and their associated WOEIDs
  ;; from twitter
  (define (twitter-locations)
    (trends-available))

  ;; Search the twitter REST API
  (define (twitter-search)
    (let ((results (read-json (search-tweets #:q "cavs" #:count 10))))
      ;; Return on the tweet information, not the application
      ;; bookkeeping indices for rate limiting
      (write-json (alist-ref 'statuses results))
      (newline)))

) ;; end of module massmine-twitter

;; twitter.scm ends here
