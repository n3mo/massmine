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

  ;; Current rate limits are controlled through set!-able
  ;; variables. Each limit variable contains a pair: (1) the number of
  ;; remaining API calls, and (2) Unix (Epoch) time until the
  ;; available calls are refreshed
  (define search-rate-limit `(0 ,(current-seconds)))
  (define trends-rate-limit `(0 ,(current-seconds)))

  ;; These values are reset with this procedure
  (define (twitter-update-rate-limits)
    (let* ((result (application-rate-limit-status #:resources "search,trends"))
	   (search-rate (flatten (alist-ref 'search (alist-ref 'resources result))))
	   (trends-rate (flatten (alist-ref 'trends (alist-ref 'resources result)))))
      (set! search-rate-limit `(,(cdr (third search-rate))
				,(cdr (fourth search-rate))))
      (set! trends-rate-limit `(,(cdr (third trends-rate))
				,(cdr (fourth trends-rate))))))

  ;; Rate limit monitor. This has side effects and sets! variables
  ;; defined above. It does so because twitter's API rate limits are
  ;; set on their end, and there is a rate limit on checking what the
  ;; current rate limits are. In other words, you can't keep hammering
  ;; twitter's api with current rate limits. Instead, this procedure
  ;; does two things: (1) First, it queries twitter's api for the
  ;; current rate limits for only those api methods used by this
  ;; module. This data is used to set! local variables used to keep
  ;; track of this info. (2) On subsequent calls to this procedure,
  ;; twitter's api is only queried when Unix (Epoch) time has expired,
  ;; and the rate limits have been refreshed. Otherwise, the local
  ;; variables are consulted. Returns #t if it's ok to query the
  ;; requested API resource, otherwise #f is returned. This procedure
  ;; can only be called with oauth, so it should only be called by
  ;; twitter tasks that have such bindings. "resource" can be either
  ;; "search" or "trends"
  (define (twitter-rate-limit resource)
    (cond
     [(equal? resource "search")
      (if search-rate-limit
	  (dosomethinghere)
	  ;; First time we've called this procedure. Query twitter
	  )]))

  (define (twitter-stream pattern geo-locations lang-code)
    (handle-exceptions exn
  	;; Exception handler does nothing but suppress the inevitable
  	;; error caused but terminating the connection manually
  	#t
      ;; This does the real work (using clucker)
      (statuses-filter #:delimited "length"
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
