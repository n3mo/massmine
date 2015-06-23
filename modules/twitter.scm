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
  (use extras irregex data-structures posix utils srfi-1)
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
  (define (twitter-update-rate-limits!)
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
     ;; REST API: Search
     [(equal? resource "search")
      (if (<= (first search-rate-limit) 0)
	  ;; We have run out of api calls (or this is the first time
	  ;; we've called this procedure, in which case the delay
	  ;; below will be zero seconds). Wait until the our rate
	  ;; limit is reset by twitter, and then continue.
	  (begin
	    ;; Update our rate limits
	    (twitter-update-rate-limits!)
	    ;; Begin waiting (if necessary)
	    (sleep (inexact->exact (max (- (second search-rate-limit) (current-seconds)))))
	    ;; We're done waiting. Query twitter to ensure that our API
	    ;; limits have been reset. Set! our rate limit variables accordingly
	    (twitter-update-rate-limits!)
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! search-rate-limit `(,(- (first search-rate-limit) 1)
				    ,(second search-rate-limit))))]
     ;; REST API: Trends
     [(equal? resource "trends")
      (if (<= (first trends-rate-limit) 0)
	  ;; We have run out of api calls (or this is the first time
	  ;; we've called this procedure, in which case the delay
	  ;; below will be zero seconds). Wait until the our rate
	  ;; limit is reset by twitter, and then continue.
	  (begin
	    ;; Update our rate limits
	    (twitter-update-rate-limits!)
	    ;; Begin waiting (if necessary)
	    (sleep (inexact->exact (max (- (second trends-rate-limit) (current-seconds)))))
	    ;; We're done waiting. Query twitter to ensure that our API
	    ;; limits have been reset. Set! our rate limit variables accordingly
	    (twitter-update-rate-limits!)
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! trends-rate-limit `(,(- (first trends-rate-limit) 1)
				    ,(second trends-rate-limit))))]))

  ;; Search the twitter streaming endpoint by keyword
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

  ;; Returns a random sample of 1% of all tweets from the streaming
  ;; endpoint. No keywords required
  (define (twitter-sample)
    (handle-exceptions exn
	;; Exception handler does nothing but suppress the inevitable
  	;; error caused but terminating the connection manually
  	#t
      (statuses-sample #:delimited "length")))

  ;; Task for retrieving list of locations and their associated WOEIDs
  ;; from twitter
  (define (twitter-locations)
    (trends-available))

  ;; Top-10 trends. Returns the current top-10 trends for a given
  ;; WOEID location. This method includes hashtags
  (define (twitter-trends woeid)
    ;; Check rate limits and pause if necessary
    (twitter-rate-limit "trends")
    (let* ((results (read-json (trends-place #:id woeid)))
	   (trends (vector->list (alist-ref 'trends (car (vector->list results)))))
	   (metadata (car (vector->list (alist-ref 'locations (car (vector->list results)))))))
      (write-json
       (list->vector
	(map (lambda (x)
	       (cons `(woeid . ,(alist-ref 'woeid metadata))
		     (cons `(location . ,(alist-ref 'name metadata)) x))) trends)))
      (newline)))

  ;; Top-10 trends. Returns the current top-10 trends for a given
  ;; WOEID location. This method EXCLUDES hashtags
  (define (twitter-trends-nohash woeid)
    ;; Check rate limits and pause if necessary
    (twitter-rate-limit "trends")
    (let* ((results (read-json (trends-place #:id woeid #:exclude "hashtags")))
	   (trends (vector->list (alist-ref 'trends (car (vector->list results)))))
	   (metadata (car (vector->list (alist-ref 'locations (car (vector->list results)))))))
      (write-json
       (list->vector
	(map (lambda (x)
	       (cons `(woeid . ,(alist-ref 'woeid metadata))
		     (cons `(location . ,(alist-ref 'name metadata)) x))) trends)))
      (newline)))

  ;; Search the twitter REST API. This is a rate-limited API endpoint,
  ;; and MUST include a call to twitter-rate-limit, which keeps track
  ;; of rate limits and sleeps an appropriate amount of time when
  ;; limits are exhausted.
  (define (twitter-search num-tweets pattern geo-locations lang-code)
    (let* ((num-counts (inexact->exact (floor (/ num-tweets 100))))
	   (raw-counts (reverse (cons (- num-tweets (* num-counts 100))
				  (take (circular-list '100) num-counts))))
	   (counts (filter (lambda (x) (not (= x 0))) raw-counts)))
      ;; counts is a list of numbers. The length of the list
      ;; corresponds to the number of times we have to query the api
      ;; (100 tweets can be returned max per query). Each individual
      ;; number corresponds to the number of requested tweets on a
      ;; given query. The sum of the list equals the requested number
      ;; of tweets from the user. E.g., if the user asked for 250
      ;; tweets, counts will be (100 100 50)

      ;; Twitter provides a mechanism for paginating results through
      ;; the parameter max_id. On the first call we supply no max_id
      ;; param. On subsequent calls, we must update and manage this
      ;; parameter to ensure that different tweets are returned on
      ;; each call
      (let query-api ((how-many counts) (max-id ""))
	(begin
	  ;; Put the brakes on if necessary
	  (twitter-rate-limit "search")
	  ;; We've passed the rate limit check, continue
	  (if (not (null? how-many))
	      (let ((results
		     (if (equal? max-id "")
			 ;; On the first query, you cannot supply a
			 ;; max_id, even the empty string ""
			 (read-json
			  (search-tweets #:q pattern
					 #:lang lang-code
					 #:geocode geo-locations
					 #:count (car how-many)))
			 ;; Subsequent queries we supply the max_id to
			 ;; handle pagination of results
			 (read-json
			  (search-tweets #:q pattern
					 #:lang lang-code
					 #:geocode geo-locations
					 #:count (car how-many)
					 #:max_id max-id)))))

		;; The remaining code should only run if we
		;; successfully received tweets on the last query. If
		;; twitter returned nothing, we're done here
		(unless (or (not results) (= (vector-length (alist-ref 'statuses results)) 0))
		  ;;We have the current batch of tweets, with meta-data
		  ;;in results. Write out the current tweets
		  (write-json (alist-ref 'statuses results))
		  (newline)
		  ;; Now bookkeeping begins. We save the current max_id
		  ;; param, substract to avoid getting the same tweet back
		  ;; on our next call. 
		  (query-api
		   (cdr how-many)
		   (- (alist-ref 'id (car
				      (reverse (vector->list (alist-ref 'statuses results)))))
		      1))))))))) 

  ;; Procedure for fetching a user's timeline. Up to 200 tweets can be
  ;; returned per api call, for a total of 3200 max. Older tweets
  ;; (>3200) cannot be retrieved from this endpoint
  (define (twitter-timeline num-tweets screen-name)
    (let* ((num-counts (inexact->exact (floor (/ num-tweets 200))))
	   (raw-counts (reverse (cons (- num-tweets (* num-counts 200))
				      (take (circular-list '200) num-counts))))
	   (counts (filter (lambda (x) (not (= x 0))) raw-counts)))
      ;; counts is a list of numbers. The length of the list
      ;; corresponds to the number of times we have to query the api
      ;; (200 tweets can be returned max per query). Each individual
      ;; number corresponds to the number of requested tweets on a
      ;; given query. The sum of the list equals the requested number
      ;; of tweets from the user. E.g., if the user asked for 550
      ;; tweets, counts will be (200 200 150)

      ;; Twitter provides a mechanism for paginating results through
      ;; the parameter max_id. On the first call we supply no max_id
      ;; param. On subsequent calls, we must update and manage this
      ;; parameter to ensure that different tweets are returned on
      ;; each call
      (let query-api ((how-many counts) (max-id ""))
	(begin
	  ;; TODO: Put the brakes on if necessary
	  ;;(twitter-rate-limit "search")
	  ;; We've passed the rate limit check, continue
	  (if (not (null? how-many))
	      (let ((results
		     (if (equal? max-id "")
			 ;; On the first query, you cannot supply a
			 ;; max_id, even the empty string ""
			 (read-json
			  (user-timeline #:screen_name screen-name
					 #:count (car how-many)
					 #:include_rts 1))
			 ;; Subsequent queries we supply the max_id to
			 ;; handle pagination of results
			 (read-json
			  (user-timeline #:screen_name screen-name
					 #:count (car how-many)
					 #:include_rts 1
					 #:max_id max-id)))))

		;; The remaining code should only run if we
		;; successfully received tweets on the last query. If
		;; twitter returned nothing, we're done here
		(unless (or (not results) (= (vector-length results) 0))
		  ;;We have the current batch of tweets, with meta-data
		  ;;in results. Write out the current tweets
		  (write-json results)
		  (newline)
		  ;; Now bookkeeping begins. We save the current max_id
		  ;; param, substract to avoid getting the same tweet back
		  ;; on our next call. 
		  (query-api
		   (cdr how-many)
		   (- (alist-ref 'id (car (reverse (vector->list results))))
		      1)))))))))


) ;; end of module massmine-twitter

;; twitter.scm ends here
