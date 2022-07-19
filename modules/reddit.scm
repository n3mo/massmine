;; ##################################################################
;;
;; MassMine: Your Access To Data
;; Copyright (C) 2014-2021  Nicholas M. Van Horn & Aaron Beveridge
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

(module massmine-reddit *

  (import (chicken base)
	  (chicken io)
	  (chicken time)
	  (chicken file)
	  (chicken string)
	  scheme
	  uri-common
	  http-client
	  intarweb
	  base64
	  srfi-1
	  srfi-13
	  medea
	  pathname-expand)
  
  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "1.3.0 (2021-01-15)" #f)))
  ;; http-client retry policy for failed connections. We could set
  ;; this to #f to make it retry indefinitely, but this will lead to
  ;; http 420 rate limit responses from Twitter. Better to be
  ;; conservative
  (max-retry-attempts 5)

  ;; Module parameters
  ;; --------------------------------------------------

  ;; Reddit API credentials file
  (define reddit-cred-file
    (make-parameter (pathname-expand "~/.config/massmine/reddit_cred")))

  ;; Bearer token for signing requests with Oauth2
  (define bearer-token (make-parameter #f))

  ;; API credential params (used only during authorization task
  (define app-id (make-parameter ""))
  (define app-secret (make-parameter ""))
  (define account-user (make-parameter ""))
  (define account-pass (make-parameter "")) 

  ;; Rate limit stuff. Reddit allows up to 60 requests per minute.
  (define reddit-requests-remaining (make-parameter 60))
  (define reddit-time-reset (make-parameter (+ 60 (current-seconds))))
  

  ;; Module information
  ;; Available tasks and brief descriptions
  (define reddit-task-descriptions
    '((reddit-auth		.       "Authenticate with Reddit")
      (reddit-search-comments	.	"Search existing comments by keyword(s)")
      (reddit-search-hot	.	"Search existing posts by keyword(s), sorted by hot")
      (reddit-search-new	.	"Search existing posts by keyword(s), sorted by new")
      (reddit-search-top	.	"Search existing posts by keyword(s), sorted by top")
      (reddit-search-relevance	.	"Search existing posts by keyword(s), sorted by relevance")))

  ;; Command line arguments supported by each task
  ;; TODO: Update this
  (define reddit-task-options
    '((reddit-auth		.       "auth")
      (reddit-search-comments	.	"query* count* date*")
      (reddit-search-hot	.	"query* count* date*")
      (reddit-search-new	.	"query* count* date*")
      (reddit-search-top	.	"query* count* date*")
      (reddit-search-relevance	.	"query* count* date*")))

  ;; Available tasks and their corresponding procedure calls
  ;; TODO: Update the correct calling method for these
  (define twitter-tasks
    '((reddit-auth		.	(reddit-setup-auth P))
      (reddit-search-hot	.       (reddit-search-hot (max-tweets) (keywords) (date)))))

  ;; Oauth management procedures
  ;; --------------------------------------------------

  ;; This unparser is needed by intarweb to add support for "bearer"
  ;; authentication
  (define (bearer-auth-param-subunparser params)
    (alist-ref 'token params))

  ;; We have to add an option for bearer tokens to intarweb
  (authorization-param-subunparsers (alist-cons 'bearer bearer-auth-param-subunparser (authorization-param-subunparsers)))
  
  ;; This returns the user's reddit credentials, if available. If the
  ;; user has not provided this information previously, an error is
  ;; reported. 
  (define (reddit-auth cred-path)
    (let ((cred-file (if cred-path cred-path (reddit-cred-file))))
      (if (file-exists? cred-file)
	  ;; Return reddit credential information
	  (with-input-from-file cred-file read)
	  ;; Else the user needs set up their credentials
	  (begin
	    (display "Authenticate before using Reddit.\nRun --> 'massmine --task=reddit-auth'\n"
		     (current-error-port))
	    (exit 1)))))


  ;; This requests a bearer token from Reddit's API. Bearer
  ;; tokens expire in 1 hour (3600 seconds). This procedure
  ;; calculates the expiration date (in seconds) and adds it to
  ;; the bearer-token alist under the key expires_on. You shouldn't
  ;; need to call this directly. Instead, you likely want to call
  ;; update-bearer-token, which handles things for you (such as
  ;; checking if the bearer-token has expired)
  (define (generate-bearer-token)
    (define reddit-cred (reddit-auth (reddit-cred-file)))
    (let ((result
	   (read-json
	    (let* ((uri (uri-reference "https://www.reddit.com/api/v1/access_token"))
		   (req (make-request method: 'POST
				      uri: uri
				      headers: (headers `((authorization #(basic
									   ((username . ,(alist-ref 'application-id reddit-cred)) 
									    (password . ,(alist-ref 'application-secret reddit-cred))))))))))
	      (with-input-from-request req
				       `((grant_type . "client_credentials") ; grant type "password" also works
					 (username . ,(alist-ref 'account-username reddit-cred))
					 (password . ,(alist-ref 'account-password reddit-cred)))
				       read-string)))))
      ;; Taken the expiration seconds and convert to an actual
      ;; time in the future at which the bearer token expires
      (alist-cons 'expires_on (+ (current-seconds) (alist-ref 'expires_in result)) result)))

  ;; Call this before running any Reddit API data request. It modifies
  ;; the module-wide parameter bearer-token to ensure that a
  ;; non-expired bearer token is available for accessing OAuth
  ;; protected API resources
  (define (update-bearer-token)
    ;; If this is the first time this is called, the parameter
    ;; `bearer-token` will be #f. In that case, request a new bearer
    ;; token. If this has already been called 1+ time prior, then an
    ;; existing bearer token will be contained in the parameter. In
    ;; that case, this procedure should check if the token has
    ;; expired. If so, request a new token. If not, this procedure
    ;; should simply return with no side effects. We actually ensure
    ;; that there are at least 5 seconds remaining before expiration,
    ;; just to be safe
    (if (bearer-token)
	(let ((seconds-remaining (- (alist-ref 'expires_on (bearer-token))
				    (current-seconds))))
	  (if (< seconds-remaining 5)
	      (bearer-token (generate-bearer-token))))
	;; There is no bearer-token yet... get one!
	(bearer-token (generate-bearer-token))))

  ;; Call this at the start of any API requests to ensure that
  ;; MassMine doesn't exceed the limits set by Reddit. Reddit allows
  ;; for up to 60 reqesuts per minute. So this procedure monitors how
  ;; many requests have been made, and well as when the first request
  ;; was made. If the maximum number of requests have been made, this
  ;; procedure will sleep until the original 60 seconds has
  ;; expired. Then it will reset the limits and return to the calling
  ;; procedure 
  (define (reddit-rate-limit)
    (if (<= (reddit-requests-remaining) 0)
	;; We reached our rate limits. Wait for a bit to reset things
	(let ((sleep-seconds (- (reddit-time-reset)
				(current-seconds))))
	  ;; Put the brakes on for a bit
	  (sleep sleep-seconds)
	  ;; Rate limits have recovered... continue on after resetting
	  ;; limit counters
	  (reddit-requests-remaining 59)
	  (reddit-time-reset (+ 60 (current-seconds))))
	;; We still have requets available. Increment counter and
	;; continue on
	(reddit-requests-remaining (- (reddit-requests-remaining)
				      1))))

  ;; This is how to use the bearer token to request data from the
  ;; API. This mimics the example script example from Reddit's
  ;; API docs
  ;;
  ;; (bearer-token (generate-bearer-token))
  ;;
  ;; (let* ((uri (uri-reference "https://oauth.reddit.com/api/v1/me"))
  ;;        (req (make-request method: 'GET
  ;; 			  uri: uri
  ;; 			  headers: (headers `((authorization
  ;; 					       #(bearer
  ;; 						 ((token . ,(alist-ref 'access_token (bearer-token)))))))))))
  ;;   (with-input-from-request req
  ;; 			   #f
  ;; 			   read-string))


  ;; --------------------------------------------------
  ;; Module general procedures
  ;; --------------------------------------------------

  ;; Helper function: get confirmation from user
  (define (yes-or-no? msg #!key default)
    (print (string-append msg " (yes/no)"))
    (let ((resp (string-trim-both (read-line))))
      (cond ((string-ci=? "yes" resp) #t)
	    (else #f))))

  ;; --------------------------------------------------
  ;; API procedures
  ;; --------------------------------------------------

  ;; These are procedures for handling API queries. These are not
  ;; procedures tied to massmine tasks. Massmine task procedures use
  ;; these to make individual calls. These functions all end in -api
  ;; to make this clear

  ;; Query search api. This function requires a valid and non-expired
  ;; bearer-token to be parameterized in bearer-token. Keyword options
  ;; match those described on Reddit's API docs at:
  ;; https://www.reddit.com/dev/api#GET_search
  (define (reddit-search-api #!key subreddit after before
			     category count include_facets limit q
			     restrict_sr show sort sr_detail t type)
    (let* ((uri (uri-reference (string-append
				(string-append "https://oauth.reddit.com/r/" subreddit "/search?")
				(form-urlencode `((after . ,after)
						  (before . ,before)
						  (category . ,category)
						  (count . ,count)
						  (include_facets . ,include_facets)
						  (limit . ,limit)
						  (q . ,q)
						  (restrict_sr . ,restrict_sr)
						  (show . ,show)
						  (sort . ,sort)
						  (sr_detail . ,sr_detail)
						  (t . ,t)
						  (type . ,type))))))
	   (req (make-request method: 'GET
			      uri: uri
			      headers: (headers `((authorization
						   #(bearer
						     ((token . ,(alist-ref 'access_token (bearer-token)))))))))))
      
      ;; Return the substantive data results
      (with-input-from-request req
			       #f
			       read-string)))


  ;; HOW TO USE THE CODE ABOVE --------------------------------------------------

  ;; (bearer-token (generate-bearer-token))

  ;; reddit-search-api returns TWO values: the first is the kind of
  ;; response (kind . "Listing"), and the second is the data (data
  ;; ...). You probably want to drop the first, at least after
  ;; checking it to ensure things worked alright. In the following
  ;; example, I just drop the first value with cdr

  ;; (define result (alist-ref 'data
  ;; 			    (cdr (read-json
  ;; 				  (reddit-search-api #:subreddit "politics"
  ;; 						     #:limit 10
  ;; 						     #:q "ukraine"
  ;; 						     #:sort "relevance"
  ;; 						     #:t "week")))))

  ;; result created with the previous command contains the following
  ;; keys/values:
  ;; 
  ;; modhash	: can ignore this since we're using oauth
  ;; dist	: the number of results returned(?)
  ;; facets	: can ignore this
  ;; after	: (string or null) controls pagination. Must pass this back to API
  ;;              on subsequent calls to page through results
  ;; geo_filter	: can ignore 
  ;; before	: (string or null) controls pagination for going
  ;;              "backwards". We shouldn't need this.
  ;; children	: (vector) contains the actual search results

  ;; --------------------------------------------------
  ;; Task procedures
  ;; --------------------------------------------------

  ;; These are massmine task functions, called when users select a
  ;; particular task.

  ;; Reddit authentication task. This need only be used once whenever
  ;; a user needs to add new Reddit API credentials to MassMine
  (define (reddit-setup-auth cred-file)
    (let ((cred-path (if cred-file cred-file (reddit-cred-file))))
      (print "Would you like to setup your Reddit credentials?")
      (print "Warning: continuing will over-write any previous credentials")
      (if (yes-or-no? "Continue? " #:default "No" #:abort #f)
	  ;; Walk the user through setting up their credentials
	  (begin
	    (print "Please visit https://www.reddit.com/prefs/apps/ to collect")
	    (print "the following information:")
	    (display "Application id: ")
	    (app-id (string-trim-both (read-line)))
	    (display "Application secret: ")
	    (app-secret (string-trim-both (read-line)))
	    (display "Reddit account user name: ")
	    (account-user (string-trim-both (read-line)))
	    (display "Reddit account password: ")
	    (account-pass (string-trim-both (read-line)))

	    ;; TODO: Verify the user's supplied credentials. This will return
	    ;; if the credentials are successfully verified, otherwise
	    ;; an exception (with an explanation to the user) will be
	    ;; raised.
	    ;; (reddit-verify-credentials #:app-id (app-id)
	    ;; 			       #:app-secret (app-secret)
	    ;; 			       #:account-user (account-user)
	    ;; 			       #:account-pass (account-pass))

	    ;; If we've made it here, the user's credentials check out.
	    ;; Prepare a proper alist and write to disk
	    (with-output-to-file cred-path
	      (lambda ()
		(write `((application-id . ,(app-id))
			 (application-secret . ,(app-secret))
			 (account-username . ,(account-user))
			 (account-password . ,(account-pass))))))
	    (print "\nAuthentication setup finished!"))
	  (print "Stopping!"))))

  ;; Search a subreddit. This is used for all reddit search tasks,
  ;; except for searching comments. However, it is not a task
  ;; procedure itself. It is used by:
  ;; reddit-search-hot, reddit-search-new, reddit-search-top, and
  ;; reddit-search-relevance
  ;; 
  ;; Example usage: (reddit-search-hot 250 "love" "news" "hot" "month")
  (define (reddit-search num-posts pattern subreddit type timebin)
    (let* ((num-counts (inexact->exact (floor (/ num-posts 100))))
	   (raw-counts (reverse (cons (- num-posts (* num-counts 100))
				      (take (circular-list '100) num-counts))))
	   (counts (filter (lambda (x) (not (= x 0))) raw-counts)))

      ;; counts is a list of numbers. The length of the list
      ;; corresponds to the number of times we have to query the api
      ;; (100 results can be returned max per query). Each individual
      ;; number corresponds to the number of requested results on a
      ;; given query. The sum of the list equals the requested number
      ;; of results from the user. E.g., if the user asked for 250
      ;; posts, counts will be (100 100 50)

      ;; Reddit provides a mechanism for paginating results through
      ;; the parameter "after". On the first call we supply no "after"
      ;; param. On subsequent calls, we must update and manage this
      ;; parameter to ensure that different posts are returned on
      ;; each call

      (let query-api ((how-many counts) (after ""))
	(begin

	  ;; Get proper API access
	  (update-bearer-token)
	  
	  ;; Put the brakes on if necessary
	  (reddit-rate-limit)
	  
	  ;; We've passed the rate limit check, continue
	  (if (not (null? how-many))
	      (let ((results
		     (if (equal? after "")
			 ;; On the first query, you cannot supply an
			 ;; after "fullname"
			 (alist-ref 'data
				    (cdr (read-json
					  (reddit-search-api #:subreddit subreddit
							     #:limit (car how-many)
							     #:q pattern
							     #:sort type
							     #:t timebin))))
			 ;; Subsequent queries we supply the "after" parameter to
			 ;; handle pagination of results
			 (alist-ref 'data
				    (cdr (read-json
					  (reddit-search-api #:subreddit subreddit
							     #:limit (car how-many)
							     #:q pattern
							     #:sort type
							     #:t timebin
							     #:after after )))))))

		;; The remaining code should only run if we
		;; successfully received posts on the last query. If
		;; reddit returned nothing, we're done here
		(unless (or (not results) (= (vector-length (alist-ref 'children results)) 0))
		  ;; We have the current batch of results, with meta-data
		  ;; in results. Write out the current posts. 
		  ;; For other API endpoints the
		  ;; results are line-oriented JSON with one record per
		  ;; line. We extract the post data from the array and
		  ;; write it one line at a time for consistency
		  (for-each
		   (lambda (post) (write-json post) (newline))
		   (vector->list (alist-ref 'children results)))

		  ;; Now bookkeeping begins. We save the current "after"
		  ;; param for use in our next API call
		  (query-api
		   (cdr how-many)
		   (alist-ref 'after results)))))))))

  ;; Search reddit hot task. 'pattern' should be 'subreddit:query'
  (define (reddit-search-hot num-posts pattern timebin)
    (let* ((tmp (string-split pattern ":"))
	   (subreddit (first tmp))
	   (query (second tmp)))
      ;; Run the task
      (reddit-search num-posts query subreddit "hot" timebin)))

  ;; Search reddit new task. 'pattern' should be 'subreddit:query'
  (define (reddit-search-new num-posts pattern timebin)
    (let* ((tmp (string-split pattern ":"))
	   (subreddit (first tmp))
	   (query (second tmp)))
      ;; Run the task
      (reddit-search num-posts query subreddit "new" timebin)))

  ;; Search reddit top task. 'pattern' should be 'subreddit:query'
  (define (reddit-search-top num-posts pattern timebin)
    (let* ((tmp (string-split pattern ":"))
	   (subreddit (first tmp))
	   (query (second tmp)))
      ;; Run the task
      (reddit-search num-posts query subreddit "top" timebin)))

  ;; Search reddit relevance task. 'pattern' should be 'subreddit:query'
  (define (reddit-search-relevance num-posts pattern timebin)
    (let* ((tmp (string-split pattern ":"))
	   (subreddit (first tmp))
	   (query (second tmp)))
      ;; Run the task
      (reddit-search num-posts query subreddit "relevance" timebin)))
  
  ) ;; End of reddit module			       
