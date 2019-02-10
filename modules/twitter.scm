;; ##################################################################
;;
;; MassMine: Your Access To Data
;; Copyright (C) 2014-2018  Nicholas M. Van Horn & Aaron Beveridge
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

(module massmine-twitter *

  (import scheme chicken)
  (use extras irregex data-structures posix utils srfi-1)
  (use openssl oauth-client uri-common rest-bind medea clucker
       http-client pathname-expand)
  (require-extension utf8 utf8-srfi-13)

  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "1.1.0 (2018-06-06)" #f)))
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

  ;; Twitter module parameters
  (define twitter-cred-file
    (make-parameter (pathname-expand "~/.config/massmine/twitter_cred")))

  ;; Clucker parameters that need setting to our needs here
  (application-rate-limit-status-reader read-json)
  (trends-available-reader generic-reader)

  ;; Available tasks and brief descriptions
  (define twitter-task-descriptions
    '((twitter-auth .           "Authenticate with Twitter")
      (twitter-followers .      "Get followers list for a specific user")
      (twitter-friends .        "Get friends list for a specific user")
      (twitter-locations .	"Available geo locations (WOEIDS)")
      (twitter-rehydrate .      "Rehydrate tweet(s) by ID number")
      (twitter-sample .		"Get random sample of tweets in real time")
      (twitter-search .		"Search existing tweets by keyword(s)")
      (twitter-stream .		"Get tweets by keyword in real time")
      (twitter-trends .		"Top-50 trends for a given location")
      (twitter-trends-nohash .	"Top-50 trends (no #hashtags)")
      (twitter-user .		"Fetch a user's timeline of tweets")))

  ;; Command line arguments supported by each task
  (define twitter-task-options
    '((twitter-auth .           "auth")
      (twitter-followers .      "user*")
      (twitter-friends .        "user*")
      (twitter-locations .	"<none>")
      (twitter-rehydrate .      "query*")
      (twitter-sample .		"count dur ('YYYY-MM-DD HH:MM:SS')")
      (twitter-search .		"query* count geo lang")
      (twitter-stream .		"query+ user+ geo+ lang count dur ('YYYY-MM-DD HH:MM:SS')")
      (twitter-trends .		"geo* (as WOEID returned by twitter-locations)")
      (twitter-trends-nohash .	"geo* (as WOEID returned by twitter-locations)")
      (twitter-user .		"user* count")))

  ;; Available tasks and their corresponding procedure calls
  (define twitter-tasks
    '((twitter-auth . (twitter-setup-auth P))
      (twitter-rehydrate . (twitter-rehydrate (keywords)))
      (twitter-stream . (twitter-stream (keywords) (locations) (language) (user-info)))
      (twitter-sample . (twitter-sample))
      (twitter-locations . (twitter-locations))
      (twitter-trends . (twitter-trends (locations)))
      (twitter-trends-nohash . (twitter-trends-nohash (locations)))
      (twitter-user . (twitter-timeline (max-tweets) (user-info)))
      (twitter-search . (twitter-search (max-tweets) (keywords) (locations) (language)))))
  
  ;; This returns the user's twitter credentials, if available. If the
  ;; user has not provided this information previously, an error is
  ;; reported 
  (define (twitter-auth cred-path)
    (let ((cred-file (if cred-path cred-path (twitter-cred-file))))
	(if (file-exists? cred-file)
	    ;; Return twitter credential information
	    (with-input-from-file cred-file read)
	    ;; Else the user needs set up their credentials
	    (begin
	      (display "Authenticate before using Twitter.\nRun --> 'massmine --task=twitter-auth'\n"
		       (current-error-port))
	      (exit 1)))))

  ;; Use this to verify the credentials supplied by the user. If this
  ;; returns, then the credentials were valid. If not, an exception is
  ;; raised warning the user about the error
  (define (twitter-verify-credentials #!key
				      consumer-key
				      consumer-secret
				      access-token
				      access-token-secret)
    (let ((twitter-app
	   (twitter-service #:consumer-key consumer-key
			    #:consumer-secret consumer-secret))
	  (user-tokens
	   (twitter-token-credential #:access-token access-token
				     #:access-token-secret access-token-secret)))
      (with-oauth
       twitter-app user-tokens
       (lambda ()
	 (handle-exceptions exn
	     (begin
	       (display "\nAn error occurred while verifying your credentials.\n" (current-error-port))
	       (display "Please double check your values and try again.\n" (current-error-port))
	       (exit 1))
	   (account-verify-credentials))))))

  ;; This walks the user through setting up their Twitter credentials
  ;; for MassMine
  (define (twitter-setup-auth cred-file)
    (let ((cred-path (if cred-file cred-file (twitter-cred-file))))
      (print "Would you like to setup your Twitter credentials?")
      (print "Warning: continuing will over-write any previous credentials")
      (if (yes-or-no? "Continue?" #:default "No" #:abort #f)
	  ;; Walk the user through setting up their credentials
	  (begin
	    (print "Please visit https://apps.twitter.com to collect")
	    (print "the following information:")
	    (display "Consumer key: ")
	    (define c-key (string-trim-both (read-line)))
	    (display "Consumer secret: ")
	    (define c-secret (string-trim-both (read-line)))
	    (display "Access token: ")
	    (define a-token (string-trim-both (read-line)))
	    (display "Access token secret: ")
	    (define a-secret (string-trim-both (read-line)))

	    ;; Verify the user's supplied credentials. This will return
	    ;; if the credentials are successfully verified, otherwise
	    ;; an exception (with an explanation to the user) will be
	    ;; raised.
	    (twitter-verify-credentials #:consumer-key c-key
					#:consumer-secret c-secret
					#:access-token a-token
					#:access-token-secret a-secret)

	    ;; If we've made it here, the user's credentials check out.
	    ;; Prepare a proper alist and write to disk
	    (with-output-to-file cred-path
	      (lambda ()
		(write `((consumer-key . ,c-key)
			 (consumer-secret . ,c-secret)
			 (access-token . ,a-token)
			 (access-token-secret . ,a-secret)))))
	    (print "\nAuthentication setup finished!"))
	  (print "Stopping!"))))

  ;; Current rate limits are controlled through set!-able
  ;; variables. Each limit variable contains a pair: (1) the number of
  ;; remaining API calls, and (2) Unix (Epoch) time until the
  ;; available calls are refreshed
  (define followers-rate-limit `(0 0))
  (define friends-rate-limit `(0 0))
  (define search-rate-limit `(0 0))
  (define statuses-rate-limit `(0 0))
  (define timeline-rate-limit `(0 0))
  (define trends-rate-limit `(0 0))
  
  ;; These values are reset with this procedure
  (define (twitter-update-rate-limits!)
    (let* ((result (application-rate-limit-status #:resources "search,trends,statuses,friends,followers"))
	   (search-rate (flatten (alist-ref 'search (alist-ref 'resources result))))
	   (statuses-rate (flatten (alist-ref 'statuses (alist-ref 'resources result))))
	   (friends-rate
	    (cons '/friends/list
		  (alist-ref '/friends/list (alist-ref 'friends (alist-ref 'resources result)))))
	   (followers-rate
	    (cons '/followers/list
		  (alist-ref '/followers/list (alist-ref 'followers (alist-ref 'resources result)))))
	   (trends-rate (flatten (alist-ref 'trends (alist-ref 'resources result))))
	   (timeline-rate
	    (alist-ref '/statuses/user_timeline (alist-ref 'statuses (alist-ref 'resources result)))))
      (set! search-rate-limit `(,(cdr (third search-rate))
				,(cdr (fourth search-rate))))
      (set! statuses-rate-limit `(,(cdr (third statuses-rate))
				,(cdr (fourth statuses-rate))))
      (set! friends-rate-limit `(,(cdr (third friends-rate))
				 ,(cdr (fourth friends-rate))))
      (set! followers-rate-limit `(,(cdr (third followers-rate))
				   ,(cdr (fourth followers-rate))))
      (set! trends-rate-limit `(,(cdr (third trends-rate))
				,(cdr (fourth trends-rate))))
      (set! timeline-rate-limit `(,(cdr (second timeline-rate))
				  ,(cdr (third timeline-rate))))))

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
  ;; variables are consulted. Returns control to the calling function
  ;; when it's "safe" to query the API resource. Until it is "safe",
  ;; the process is stalled with sleep until the resource is
  ;; available. This procedure can only be called with oauth, so it
  ;; should only be called by twitter tasks that have such
  ;; bindings. "resource" can be either "search", "statuses",
  ;; "friends", "followers", "trends", or "timeline"
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
	    (if (<= (first search-rate-limit) 0)
		(begin
		  (sleep (inexact->exact (max (- (second search-rate-limit) (current-seconds)))))
		  ;; We're done waiting. Query twitter to ensure that our API
		  ;; limits have been reset. Set! our rate limit variables accordingly
		  (twitter-update-rate-limits!)))
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! search-rate-limit `(,(- (first search-rate-limit) 1)
				    ,(second search-rate-limit))))]
     ;; REST API: Statuses
     [(equal? resource "statuses")
      (if (<= (first statuses-rate-limit) 0)
	  ;; We have run out of api calls (or this is the first time
	  ;; we've called this procedure, in which case the delay
	  ;; below will be zero seconds). Wait until the our rate
	  ;; limit is reset by twitter, and then continue.
	  (begin
	    ;; Update our rate limits
	    (twitter-update-rate-limits!)
	    ;; Begin waiting (if necessary)
	    (if (<= (first statuses-rate-limit) 0)
		(begin
		  (sleep (inexact->exact (max (- (second statuses-rate-limit) (current-seconds)))))
		  ;; We're done waiting. Query twitter to ensure that our API
		  ;; limits have been reset. Set! our rate limit variables accordingly
		  (twitter-update-rate-limits!)))
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! statuses-rate-limit `(,(- (first statuses-rate-limit) 1)
				    ,(second statuses-rate-limit))))]
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
	    (if (<= (first trends-rate-limit) 0)
		(begin
		  (sleep (inexact->exact (max (- (second trends-rate-limit) (current-seconds)))))
		  ;; We're done waiting. Query twitter to ensure that our API
		  ;; limits have been reset. Set! our rate limit variables accordingly
		  (twitter-update-rate-limits!)))
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! trends-rate-limit `(,(- (first trends-rate-limit) 1)
				    ,(second trends-rate-limit))))]
     ;; REST API: Friends
     [(equal? resource "friends")
      (if (<= (first friends-rate-limit) 0)
	  ;; We have run out of api calls (or this is the first time
	  ;; we've called this procedure, in which case the delay
	  ;; below will be zero seconds). Wait until the our rate
	  ;; limit is reset by twitter, and then continue.
	  (begin
	    ;; Update our rate limits
	    (twitter-update-rate-limits!)
	    ;; Begin waiting (if necessary)
	    (if (<= (first friends-rate-limit) 0)
		(begin
		  (sleep (inexact->exact (max (- (second friends-rate-limit) (current-seconds)))))
		  ;; We're done waiting. Query twitter to ensure that our API
		  ;; limits have been reset. Set! our rate limit variables accordingly
		  (twitter-update-rate-limits!)))
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! friends-rate-limit `(,(- (first friends-rate-limit) 1)
				     ,(second friends-rate-limit))))]
     ;; REST API: Followers
     [(equal? resource "followers")
      (if (<= (first followers-rate-limit) 0)
	  ;; We have run out of api calls (or this is the first time
	  ;; we've called this procedure, in which case the delay
	  ;; below will be zero seconds). Wait until the our rate
	  ;; limit is reset by twitter, and then continue.
	  (begin
	    ;; Update our rate limits
	    (twitter-update-rate-limits!)
	    ;; Begin waiting (if necessary)
	    (if (<= (first followers-rate-limit) 0)
		(begin
		  (sleep (inexact->exact (max (- (second followers-rate-limit) (current-seconds)))))
		  ;; We're done waiting. Query twitter to ensure that our API
		  ;; limits have been reset. Set! our rate limit variables accordingly
		  (twitter-update-rate-limits!)))
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! followers-rate-limit `(,(- (first followers-rate-limit) 1)
				       ,(second followers-rate-limit))))]
     ;; REST API: User timelines
     [(equal? resource "timeline")
      (if (<= (first timeline-rate-limit) 0)
	  ;; We have run out of api calls (or this is the first time
	  ;; we've called this procedure, in which case the delay
	  ;; below will be zero seconds). Wait until the our rate
	  ;; limit is reset by twitter, and then continue.
	  (begin
	    ;; Update our rate limits
	    (twitter-update-rate-limits!)
	    ;; Begin waiting (if necessary)
	    (if (<= (first timeline-rate-limit) 0)
		(begin
		  (sleep (inexact->exact (max (- (second timeline-rate-limit) (current-seconds)))))
		  ;; We're done waiting. Query twitter to ensure that our API
		  ;; limits have been reset. Set! our rate limit variables accordingly
		  (twitter-update-rate-limits!)))
	    ;; Try again
	    (twitter-rate-limit resource))
	  ;; We have api calls available. Decrement our counter and
	  ;; return to sender
	  (set! timeline-rate-limit `(,(- (first timeline-rate-limit) 1)
				      ,(second timeline-rate-limit))))]))

  ;; Search the twitter streaming endpoint by keyword
  (define (twitter-stream pattern geo-locations lang-code user-ids)
    (handle-exceptions exn
	;; The clucker streaming reader closes the connection to
	;; Twitter's API crudely, causing a guaranteed error. We catch
	;; this harmless error but pass along any others...
  	(if (equal?
	     ((condition-property-accessor 'exn 'message) exn) "port already closed")
	    #t (abort exn))
      ;; This does the real work (using clucker)
      (statuses-filter #:delimited "length"
			      #:track pattern
			      #:locations geo-locations
			      #:language lang-code
			      #:follow user-ids)))

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

  ;; Top-50 trends. Returns the current top-50 trends for a given
  ;; WOEID location. This method includes hashtags
  (define (twitter-trends woeid)
    ;; Check rate limits and pause if necessary
    (twitter-rate-limit "trends")
    (let* ((results (read-json (trends-place #:id woeid)))
	   (trends (vector->list (alist-ref 'trends (car (vector->list results)))))
	   (metadata (car (vector->list (alist-ref 'locations (car (vector->list results))))))
	   (full-data
	    (map (lambda (x)
		   (cons `(timestamp . ,(time->string (seconds->local-time (current-seconds))))
			 (cons `(woeid . ,(alist-ref 'woeid metadata))
			       (cons `(location . ,(alist-ref 'name metadata)) x)))) trends)))
      ;; Write each trend as it's own JSON line
      (for-each (lambda (trend) (write-json trend) (newline))
		full-data)))

  ;; Top-50 trends. Returns the current top-50 trends for a given
  ;; WOEID location. This method EXCLUDES hashtags
  (define (twitter-trends-nohash woeid)
    ;; Check rate limits and pause if necessary
    (twitter-rate-limit "trends")
    (let* ((results (read-json (trends-place #:id woeid #:exclude "hashtags")))
	   (trends (vector->list (alist-ref 'trends (car (vector->list results)))))
	   (metadata (car (vector->list (alist-ref 'locations (car (vector->list results))))))
	   (full-data 
	    (map (lambda (x)
		   (cons `(timestamp . ,(time->string (seconds->local-time (current-seconds))))
			 (cons `(woeid . ,(alist-ref 'woeid metadata))
			       (cons `(location . ,(alist-ref 'name metadata)) x)))) trends)))
      ;; Write each trend as it's own JSON line
      (for-each (lambda (trend) (write-json trend) (newline))
		full-data)))

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
		  ;;in results. Write out the current tweets. The old
		  ;;method wrote the results as-is, which happens to
		  ;;be a JSON array. For other API endpoints the
		  ;;results are line-oriented JSON with one tweet per
		  ;;line. We extract the tweet data from the array and
		  ;;write it one line at a time for consistency
		  (for-each
		   (lambda (tweet) (write-json tweet) (newline))
		   (vector->list (alist-ref 'statuses results)))

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
	   (counts (filter (lambda (x) (not (= x 0))) raw-counts))
	   (user-names (map string-trim-both (string-split screen-name ","))))
      ;; twitter-timeline accepts either a single user name, or
      ;; multiple user names separated by commas. Up to num-tweets of
      ;; records are returned for each user successively
      (for-each (lambda (curr-user)
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
		      ;; Put the brakes on if necessary
		      (twitter-rate-limit "timeline")
		      ;; We've passed the rate limit check, continue
		      (if (not (null? how-many))
			  (let ((results
				 (if (equal? max-id "")
				     ;; On the first query, you cannot supply a
				     ;; max_id, even the empty string ""
				     (read-json
				      (statuses-user-timeline #:screen_name curr-user
							      #:count (car how-many)
							      #:include_rts 1))
				     ;; Subsequent queries we supply the max_id to
				     ;; handle pagination of results
				     (read-json
				      (statuses-user-timeline #:screen_name curr-user
							      #:count (car how-many)
							      #:include_rts 1
							      #:max_id max-id)))))

			    ;; The remaining code should only run if we
			    ;; successfully received tweets on the last query. If
			    ;; twitter returned nothing, we're done here
			    (unless (or (not results) (= (vector-length results) 0))
			      ;;We have the current batch of tweets, with meta-data
			      ;;in results. Write out the current tweets

			      ;; Line-oriented method
			      (for-each
			       (lambda (tweet) (write-json tweet) (newline))
			       (vector->list results))

			      ;; Now bookkeeping begins. We save the current max_id
			      ;; param, substract to avoid getting the same tweet back
			      ;; on our next call. 
			      (query-api
			       (cdr how-many)
			       (- (alist-ref 'id (car (reverse (vector->list results))))
				  1))))))))
		user-names)))

  ;; Retrieves the entire friends list for a specified user
  (define (twitter-friends-list user-name)
    ;; Initial cursor set to -1 per twitter docs (-1 requests first
    ;; "page" of results)
    (let loop ((cursor -1))
      ;; Put the brakes on if necessary
      (twitter-rate-limit "friends")
      ;; Get down to business  
      (let ((results (read-json (friends-list #:screen_name user-name
					      #:count 200
					      #:cursor cursor))))
	(for-each (lambda (user-data)
		    (write-json user-data)
		    (newline))
		  (vector->list (alist-ref 'users results)))
	(unless (= (alist-ref 'next_cursor results) 0)
	    (loop (alist-ref 'next_cursor results))))))

  ;; Retrieves the entire list of users that a specified user is
  ;; following 
  (define (twitter-followers-list user-name)
    ;; Initial cursor set to -1 per twitter docs (-1 requests first
    ;; "page" of results)
    (let loop ((cursor -1))
      ;; Put the brakes on if necessary
      (twitter-rate-limit "followers")
      ;; Get down to business  
      (let ((results (read-json (followers-list #:screen_name user-name
						#:count 200
						#:cursor cursor))))
	(for-each (lambda (user-data)
		    (write-json user-data)
		    (newline))
		  (vector->list (alist-ref 'users results)))
	(unless (= (alist-ref 'next_cursor results) 0)
	  (loop (alist-ref 'next_cursor results))))))

  ;; Rehydrate tweets. This endpoint is known as "statuses lookup" on
  ;; Twitter's API. It is more commonly referred to as "rehydrating"
  ;; with researchers, so we adopt that terminology here. Twitter
  ;; allows for 100 tweet ids to be rehydrated per call to the API,
  ;; hence the use of 100 below.
  (define (twitter-rehydrate ids)
    ;; Helper function: Splits a list into groups of length n, with
    ;; remainder list items (< n) falling into the last group. Order
    ;; is retained. A list of lists is returned
    (define (split-and-group lst n)
      (if (<= (length lst) n)
	  (cons lst '())
	  (let-values (([beg end] (split-at lst n)))
	    (cons beg (split-and-group end n)))))
    ;; Begin processing ids
    (let* ([raw-ids (map string-trim-both (string-split ids ","))]
	   [ids-list (map (lambda (x) (string-join x ","))
			  (split-and-group raw-ids 100))])
      ;; Now we get down to business. ids-list, created above is a
      ;; list of tweet ids. Each element in id-lists is a string of up
      ;; to 100 tweet ids separated by commas. Each of these strings
      ;; must be passed to statuses-lookup. Each batch of ids is read
      ;; and converted to line-oriented JSON, then written to
      ;; current-output-port
      (for-each
       (lambda (id-string)
	 ;; Put the brakes on if necessary
	 (twitter-rate-limit "statuses")
	 (let ([results (read-json (statuses-lookup #:id id-string
						    #:include_entities #t))])
	   (for-each (lambda (tweet)
		       (write-json tweet)
		       (newline))
		     (vector->list results))))
       ids-list)))
  

) ;; end of module massmine-twitter

;; twitter.scm ends here
