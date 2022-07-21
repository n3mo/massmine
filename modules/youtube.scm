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

(module massmine-youtube *

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
  ;; http 420 rate limit responses from the server. Better to be
  ;; conservative
  (max-retry-attempts 5)

  ;; URL encoding requires & rather than the default of ;
  (form-urlencoded-separator "&;")

  ;; Module parameters
  ;; --------------------------------------------------

  ;; YouTube API credentials file
  (define youtube-cred-file
    (make-parameter (pathname-expand "~/.config/massmine/youtube_cred")))

  ;; YouTube API Key. This updated from disk using the
  ;; youtube-cred-file path before any API calls can be made
  (define youtube-api-key (make-parameter #f))

  ;; Rate limit stuff. YouTube allows up to 60 requests per minute.
  (define youtube-requests-remaining (make-parameter 60))
  (define youtube-time-reset (make-parameter (+ 60 (current-seconds))))

  ;; Module information
  ;; Available tasks and brief descriptions
  (define youtube-task-descriptions
    '((youtube-auth		.       "Authenticate with Youtube")
      (youtube-video-comments   .       "Retrieve comments thread for specified video ID")
      (youtube-channel-comments .       "Retrieve comments thread for specified channel ID")))

  ;; Command line arguments supported by each task
  ;; TODO: Update this
  (define youtube-task-options
    '((youtube-auth		.       "auth")
      (youtube-video-comments   .       "query* count*")
      (youtube-channel-comments .       "query* count*")))

  ;; Available tasks and their corresponding procedure calls
  ;; TODO: Update the correct calling method for these
  (define youtube-tasks
    '((youtube-auth		.	(youtube-setup-auth P))
      (youtube-video-comments   .       (youtube-video-comments (max-tweets) (keywords)))
      (youtube-channel-comments .       (youtube-channel-comments (max-tweets) (keywords)))))

  ;; Authorization management procedures
  ;; --------------------------------------------------

  ;; This returns the user's youtube credentials, if available. If the
  ;; user has not provided this information previously, an error is
  ;; reported. 
  (define (youtube-auth cred-path)
    (let ((cred-file (if cred-path cred-path (youtube-cred-file))))
      (if (file-exists? cred-file)
	  ;; Return youtube credential information
	  (with-input-from-file cred-file read)
	  ;; Else the user needs set up their credentials
	  (begin
	    (display "Authenticate before using YouTube tasks.\nRun --> 'massmine --task=youtube-auth'\n"
		     (current-error-port))
	    (exit 1)))))

  ;; Call this at the start of any API requests to ensure that
  ;; MassMine doesn't exceed the limits set by Youtube. Youtube allows
  ;; for up to 100 reqesuts per 100 seconds. So this procedure
  ;; monitors how many requests have been made, and well as when the
  ;; first request was made. If the maximum number of requests have
  ;; been made, this procedure will sleep until the original 60
  ;; seconds has expired. Then it will reset the limits and return to
  ;; the calling procedure
  (define (youtube-rate-limit)
    (if (<= (youtube-requests-remaining) 0)
	;; We reached our rate limits. Wait for a bit to reset things
	(let ((sleep-seconds (- (youtube-time-reset)
				(current-seconds))))
	  ;; Put the brakes on for a bit
	  (sleep sleep-seconds)
	  ;; Rate limits have recovered... continue on after resetting
	  ;; limit counters
	  (youtube-requests-remaining 59)
	  (youtube-time-reset (+ 60 (current-seconds))))
	;; We still have requets available. Increment counter and
	;; continue on
	(youtube-requests-remaining (- (youtube-requests-remaining)
				      1))))

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

  ;; Request comment threads for a specified YouTube video ID from the
  ;; YouTube API. This uses the "CommentThreads: list" endpoint:
  ;; https://developers.google.com/youtube/v3/docs/commentThreads/list
  (define (youtube-video-comments-api #!key part videoId maxResults
				      textFormat pageToken key)
    (let* ((uri (uri-reference (string-append
				"https://youtube.googleapis.com/youtube/v3/commentThreads?"
				(form-urlencode `((part . ,part)
						  (videoId . ,videoId)
						  (maxResults . ,maxResults)
						  (textFormat . ,textFormat)
						  (pageToken . ,pageToken)
						  (key . ,key))))))
	   (req (make-request method: 'GET
			      uri: uri)))
      
      ;; Return the substantive data results
      (with-input-from-request req
			       #f
			       read-string)))

  ;; Request comment threads for a specified YouTube channel ID from the
  ;; YouTube API. This uses the "CommentThreads: list" endpoint:
  ;; https://developers.google.com/youtube/v3/docs/commentThreads/list
  (define (youtube-channel-comments-api #!key part allThreadsRelatedToChannelId maxResults
					textFormat pageToken key)
    (let* ((uri (uri-reference (string-append
				"https://youtube.googleapis.com/youtube/v3/commentThreads?"
				(form-urlencode `((part . ,part)
						  (allThreadsRelatedToChannelId . ,allThreadsRelatedToChannelId)
						  (maxResults . ,maxResults)
						  (textFormat . ,textFormat)
						  (pageToken . ,pageToken)
						  (key . ,key))))))
	   (req (make-request method: 'GET
			      uri: uri)))
      
      ;; Return the substantive data results
      (with-input-from-request req
			       #f
			       read-string)))

  ;; --------------------------------------------------
  ;; Task procedures
  ;; --------------------------------------------------

  ;; These are massmine task functions, called when users select a
  ;; particular task.

  ;; Youtube authentication task. This need only be used once whenever
  ;; a user needs to add new Youtube API credentials to MassMine
  (define (youtube-setup-auth cred-file)
    (let ((cred-path (if cred-file cred-file (youtube-cred-file))))
      (print "Would you like to setup your Youtube credentials?")
      (print "Warning: continuing will over-write any previous credentials")
      (if (yes-or-no? "Continue? " #:default "No" #:abort #f)
	  ;; Walk the user through setting up their credentials
	  (begin
	    (print "Please visit https://console.cloud.google.com/apis/credentials to collect")
	    (print "the following information:")
	    (display "YouTube API key: ")
	    (youtube-api-key (string-trim-both (read-line)))

	    ;; Prepare a proper alist and write to disk
	    (with-output-to-file cred-path
	      (lambda ()
		(write `((api-key . ,(youtube-api-key))))))
	    (print "\nAuthentication setup finished!"))
	  (print "Stopping!"))))

  ;; Youtube video comments. Returns youtube video comments thread for
  ;; a specified video ID. Example video ID: atZqRbpb5gg
  (define (youtube-video-comments num-posts videoID)
    (let* ((num-counts (inexact->exact (floor (/ num-posts 100))))
	   (raw-counts (reverse (cons (- num-posts (* num-counts 100))
				      (take (circular-list '100) num-counts))))
	   (counts (filter (lambda (x) (not (= x 0))) raw-counts)))

      ;; Grab the user's API key
      (youtube-api-key (alist-ref 'api-key (youtube-auth (youtube-cred-file))))

      ;; counts is a list of numbers. The length of the list
      ;; corresponds to the number of times we have to query the api
      ;; (100 results can be returned max per query). Each individual
      ;; number corresponds to the number of requested results on a
      ;; given query. The sum of the list equals the requested number
      ;; of results from the user. E.g., if the user asked for 250
      ;; posts, counts will be (100 100 50)

      ;; Youtube provides a mechanism for paginating results through
      ;; the parameter "pageToken". On the first call we supply no
      ;; "pageToken" param. On subsequent calls, we must update and
      ;; manage this parameter to ensure that different posts are
      ;; returned on each call

      (let query-api ((how-many counts) (after ""))
	(begin

	  ;; Put the brakes on if necessary
	  (youtube-rate-limit)
	  
	  ;; We've passed the rate limit check, continue
	  (if (not (null? how-many))
	      (let ((results
		     (if (equal? after "")
			 ;; On the first query, you cannot supply an
			 ;; after "fullname"
			 (read-json
			  (youtube-video-comments-api #:part "id,replies,snippet"
						      #:videoId videoID
						      #:maxResults (car how-many)
						      #:key (youtube-api-key)))
			 ;; Subsequent queries we supply the "pageToken" parameter to
			 ;; handle pagination of results
			 (read-json
			  (youtube-video-comments-api #:part "id,replies,snippet"
						      #:videoId videoID
						      #:maxResults (car how-many)
						      #:pageToken after
						      #:key (youtube-api-key))))))

		;; The remaining code should only run if we
		;; successfully received posts on the last query. If
		;; youtube returned nothing, we're done here
		(unless (or (not results) (= (vector-length (alist-ref 'items results)) 0))
		  ;; We have the current batch of results.  Write out
		  ;; the current posts.  For other API endpoints the
		  ;; results are line-oriented JSON with one record
		  ;; per line. We extract the post data from the array
		  ;; and write it one line at a time for consistency
		  (for-each
		   (lambda (post) (write-json post) (newline))
		   (vector->list (alist-ref 'items results)))

		  ;; Now bookkeeping begins. We save the current "after"
		  ;; param for use in our next API call
		  (query-api
		   (cdr how-many)
		   (alist-ref 'nextPageToken results)))))))))

  ;; Youtube channel comments. Returns youtube channel comments thread for
  ;; a specified video ID. Example channel ID: UCHP9CdeguNUI-_nBv_UXBhw
  (define (youtube-channel-comments num-posts channelID)
    (let* ((num-counts (inexact->exact (floor (/ num-posts 100))))
	   (raw-counts (reverse (cons (- num-posts (* num-counts 100))
				      (take (circular-list '100) num-counts))))
	   (counts (filter (lambda (x) (not (= x 0))) raw-counts)))

      ;; Grab the user's API key
      (youtube-api-key (alist-ref 'api-key (youtube-auth (youtube-cred-file))))

      ;; counts is a list of numbers. The length of the list
      ;; corresponds to the number of times we have to query the api
      ;; (100 results can be returned max per query). Each individual
      ;; number corresponds to the number of requested results on a
      ;; given query. The sum of the list equals the requested number
      ;; of results from the user. E.g., if the user asked for 250
      ;; posts, counts will be (100 100 50)

      ;; Youtube provides a mechanism for paginating results through
      ;; the parameter "pageToken". On the first call we supply no
      ;; "pageToken" param. On subsequent calls, we must update and
      ;; manage this parameter to ensure that different posts are
      ;; returned on each call

      (let query-api ((how-many counts) (after ""))
	(begin

	  ;; Put the brakes on if necessary
	  (youtube-rate-limit)
	  
	  ;; We've passed the rate limit check, continue
	  (if (not (null? how-many))
	    (let ((results
		   (if (equal? after "")
		     ;; On the first query, you cannot supply an
		     ;; after "fullname"
		     (read-json
		      (youtube-channel-comments-api #:part "id,replies,snippet"
						    #:allThreadsRelatedToChannelId channelID
						    #:maxResults (car how-many)
						    #:key (youtube-api-key)))
		     ;; Subsequent queries we supply the "pageToken" parameter to
		     ;; handle pagination of results
		     (read-json
		      (youtube-channel-comments-api #:part "id,replies,snippet"
						    #:allThreadsRelatedToChannelId channelID
						    #:maxResults (car how-many)
						    #:pageToken after
						    #:key (youtube-api-key))))))

	      ;; The remaining code should only run if we
	      ;; successfully received posts on the last query. If
	      ;; youtube returned nothing, we're done here
	      (unless (or (not results) (= (vector-length (alist-ref 'items results)) 0))
		;; We have the current batch of results.  Write out
		;; the current posts.  For other API endpoints the
		;; results are line-oriented JSON with one record
		;; per line. We extract the post data from the array
		;; and write it one line at a time for consistency
		(for-each
		  (lambda (post) (write-json post) (newline))
		  (vector->list (alist-ref 'items results)))

		;; Now bookkeeping begins. We save the current "after"
		;; param for use in our next API call
		(query-api
		 (cdr how-many)
		 (alist-ref 'nextPageToken results)))))))))
  
  ) ;; End of youtube module			       
