;;; clucker.scm --- Twitter API Acccess Egg For Chicken Scheme
;; Copyright 2014, Nicholas M. Van Horn
;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: twitter rest API scheme chicken
;; Version: 0.1
;;;;;

;;; TODO:
;;; + URL encode requests (or does rest-bind do this alread?) 
;;;   https://en.wikipedia.org/wiki/Percent-encoding

(module clucker *

  (import scheme chicken)
  (use extras irregex data-structures)
  (use openssl oauth-client uri-common rest-bind medea)

  ;; Lots of web services, including Twitter, don't accept ';' separated
  ;; query strings so use '&' for encoding by default but support both
  ;; '&' and ';' for decoding.
  (form-urlencoded-separator "&;")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Helper Procedures
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; These procedures are useful for working with data retrieved from
  ;; Twitter's API and/or for using the clucker egg.

  ;; Taken from my "s" egg
  (define (s-replace old new s)
    (irregex-replace/all (irregex-quote old) s new))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Custom Readers
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; These readers can be passed to methods created with rest-bind's
  ;; define-method to parse the retuned json into csv-friendly arrays.
  ;; Each custom reader needs its own header, which is a procedure that
  ;; returns a list of keys to be matched using alist-ref. These keys
  ;; will become the column names in tabular csv data. The order of the
  ;; header keys determines the order of the csv columns.

  ;; (define (search-twitter-reader result)
  ;;   (search-twitter->record (read-json result)))

  ;; (define (trends-place-header)
  ;;   '(name query url promoted_content woeid))

  ;; (define (trends-place-reader result)
  ;;   (trends-place->record (read-json result)))

  ;; This reader monitors time AND the number of tweets collected
  ;; (define max-tweets 10)
  ;; (define global-max-seconds 30)
  (define max-tweets)
  (define global-max-seconds)

  (define (statuses-filter-reader result)
    (let ((max-seconds (+ (current-seconds) global-max-seconds)))
      (let lp ((line (read-line result))
	       (num-tweets 0))
	(if (and (<= num-tweets max-tweets) (< (current-seconds) max-seconds))
	    (unless (eof-object? line)
	      (cond ((not (string->number line))
		     (begin
		       (display line)
		       (newline)
		       (lp (read-line result) (+ num-tweets 1))))
		    (else
		     (lp (read-line result) num-tweets))))
	    (close-input-port result)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; API Access Methods
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; You are unlikley to want to call these methods directly. They make
  ;; the calls to Twitter's API, but they do not manage such things as
  ;; pagniation of results and rate limits. Use the convenience
  ;; procedures below instead of these to simplify these steps.

  ;; Fetch a user's timeline
  ;; (define-method (user-timeline-method #!key
  ;; 				       user_id
  ;; 				       screen_name
  ;; 				       since_id
  ;; 				       count
  ;; 				       max_id
  ;; 				       trim_user
  ;; 				       exclude_replies
  ;; 				       contributor_details
  ;; 				       include_rts)
  ;;   "https://api.twitter.com/1.1/statuses/user_timeline.json"
  ;;   #f user-timeline-reader #f)

  ;; ;;; Search the rest API
  ;; (define-method (search-twitter-method #!key
  ;; 					q
  ;; 					geocode
  ;; 					lang
  ;; 					locale
  ;; 					result_type
  ;; 					count
  ;; 					until
  ;; 					since_id
  ;; 					max_id
  ;; 					include_entities
  ;; 					callback)
  ;;   "https://api.twitter.com/1.1/search/tweets.json"
  ;;   #f search-twitter-reader #f)

  ;; (define-method (trends-place-method #!key id exclude)
  ;;   "https://api.twitter.com/1.1/trends/place.json"
  ;;   #f trends-place-reader #f)

  ;; (define-method (application-rate-limit-status-method #!key resources)
  ;;   "https://api.twitter.com/1.1/application/rate_limit_status.json"
  ;;   #f read-json #f)

  ;; Streaming API. This method runs forever, unless the global
  ;; parameters max-tweets or global-max-seconds are set to
  ;; something. If so, the statuses-filter-reader kills the connection
  ;; (rather crudely, due to limitations in chicken's http package
  (define-method (statuses-filter-method #!key delimited stall_warnings
					 follow track locations)
    "https://stream.twitter.com/1.1/statuses/filter.json"
    #f statuses-filter-reader #f)


  ;; (define-method (debug-method #!key id exclude)
  ;;   "https://api.twitter.com/1.1/search/tweets.json"
  ;;   #f read-json #f)

  ;; Macro for building API methods
  ;; (define-syntax twitter-method
  ;;   (syntax-rules ()
  ;;     ((twitter-method method-name
  ;; 		     url
  ;; 		     body-writer
  ;; 		     body-reader
  ;; 		     header-reader)
  ;;      (lambda ()`(define-method (,method-name #!rest)
  ;; 		  (string-append
  ;; 		   "https://api.twitter.com/1.1/"
  ;; 		   url
  ;; 		   ".json")
  ;; 		  body-writer
  ;; 		  body-reader
  ;; 		  header-reader)))))

  ;; (define-syntax make-twitter-method
  ;;   (ir-macro-transformer
  ;;    (lambda (form inject compare?)
  ;;      (let* ((args (cdr form))
  ;; 	    (method-name (first args))
  ;; 	    (url (second args)))
  ;;        `(define-method (,method-name)
  ;; 	  (string-append
  ;; 	   "https://api.twitter.com/1.1/"
  ;; 	   ,url
  ;; 	   ".json")
  ;; 	  #f
  ;; 	  twitter-reader
  ;; 	  #f)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Clucker Procedures
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; These procedures are meant for direct use in building applications
  ;; using clucker. These follow the naming covention of removing the
  ;; suffix "-method" from their corresponding API Access Methods from
  ;; above. Keys offer optional API control (defaults follow Twitter's
  ;; documentation at https://dev.twitter.com/rest/public. Every
  ;; procedure requires as the first parameter "credentials" This is a
  ;; valid list as returned by make-clucker-credential

  ;; Search Twitter's rest API.
  ;; (define (search-twitter query #!key count)
  ;;   (search-twitter-method q: query count: count))

  ;; Work in progress below
  ;; (define (search-twitter #!key
  ;; 			q
  ;; 			geocode
  ;; 			lang
  ;; 			locale
  ;; 			result_type
  ;; 			count
  ;; 			until
  ;; 			since_id
  ;; 			max_id
  ;; 			include_entities
  ;; 			callback)
  ;;   (let* ((tmp (application-rate-limit-status-method resources:
  ;; 						     "search"))
  ;; 	 (rate (cdr (car (cdr (car (alist-ref 'resources rate))))))
  ;; 	 (limit (alist-ref 'limit rate))
  ;; 	 (remain (alist-ref 'remaining rate)))
  ;;     (let search-loop ((limit remain))
  ;;       )))
  
  ) ;; end of module clucker

;;; clucker.scm ends here
