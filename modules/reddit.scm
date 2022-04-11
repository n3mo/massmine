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

  (import (chicken io)
	  (chicken time)
	  uri-common
	  http-client
	  intarweb
	  base64
	  srfi-1
	  medea)
  
  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "1.3.0 (2021-01-15)" #f)))
  ;; http-client retry policy for failed connections. We could set
  ;; this to #f to make it retry indefinitely, but this will lead to
  ;; http 420 rate limit responses from Twitter. Better to be
  ;; conservative
  (max-retry-attempts 5)

  ;; Sometimes ssl shutdown operations take a little extra time to
  ;; shutdown, leading to improperly closed connections (and
  ;; crashes). The default timeout occurs at 2 minutes which can lead
  ;; to problems. We increase it here. Times for this parameter are
  ;; specified in msecs
  (ssl-shutdown-timeout 240000)		; 4 minutes

  ;; Module parameters
  ;; --------------------------------------------------

  ;; Bearer token for signing requests with Oauth2
  (define bearer-token (make-parameter #f))

  ;; This unparser is needed by intarweb to add support for "bearer"
  ;; authentication
  (define (bearer-auth-param-subunparser params)
    (alist-ref 'token params))

  ;; We have to add an option for bearer tokens to intarweb
  (authorization-param-subunparsers (alist-cons 'bearer bearer-auth-param-subunparser (authorization-param-subunparsers)))
  
  ;; This requests a bearer token from Reddit's API. Bearer
  ;; tokens expire in 1 hour (3600 seconds). This procedure
  ;; calculates the expiration date (in seconds) and adds it to
  ;; the bearer-token alist under the key expires_on
  (define (generate-bearer-token)
    (let ((result
	   (read-json
	    (let* ((uri (uri-reference "https://www.reddit.com/api/v1/access_token"))
		   (req (make-request method: 'POST
				      uri: uri
				      headers: (headers '((authorization #(basic
									   ((username . "tUCIuSMLU5uUYQ") 
									    (password . "1DnPNmo_M_2jfJ2Gr5LRhewkFe0")))))))))
	      (with-input-from-request req
				       '((grant_type . "password")
					 (username . "massmine_dev")
					 (password . "I<bR=B5,b)g^swb?F{K29S2Q("))
				       read-string)))))
      ;; Taken the expiration seconds and convert to an actual
      ;; time in the future at which the bearer token expires
      (alist-cons 'expires_on (+ (current-seconds) (alist-ref 'expires_in result)) result)))

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

  ;; Search a subreddit.
  ;; Example usage: (reddit-search 250 "love" "news" "hot" "month")
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
	  ;; Put the brakes on if necessary

	  ;; TODO: check the status of our bearer-token to see if we
	  ;; need to request a new one

	  ;; TODO: check rate limits to see if we're within the 60 API
	  ;; requests per minute allowed
	  
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
  
  ) ;; End of reddit module			       
