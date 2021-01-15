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

;; After users create an app with their Tumblr account, they must
;; visit https://api.tumblr.com/console/calls/user/info to generate
;; their token and token secret

(module massmine-tumblr *

  (import scheme)
  (import (chicken base) (chicken file) (chicken condition)
	  (chicken io) (chicken string) (chicken time))
  (import srfi-13)
  (import openssl)
  (import medea oauth-client uri-common http-client pathname-expand)

  (form-urlencoded-separator "&;")

  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "1.3.0 (2021-01-15)" #f)))

  ;; http-client retry policy for failed connections. We could set
  ;; this to #f to make it retry indefinitely, but better to be
  ;; conservative
  (max-retry-attempts 5)

  ;; Sometimes ssl shutdown operations take a little extra time to
  ;; shutdown, leading to improperly closed connections (and
  ;; crashes). The default timeout occurs at 2 minutes which can lead
  ;; to problems. We increase it here. Times for this parameter are
  ;; specified in msecs
  (ssl-shutdown-timeout 240000)		; 4 minutes

  ;; Tumblr module parameters
  (define tumblr-cred-file
    (make-parameter (pathname-expand "~/.config/massmine/tumblr_cred")))

  ;; Available tasks and brief descriptions
  (define tumblr-task-descriptions
    '((tumblr-auth .           "Authenticate with Tumblr")
      (tumblr-blog-info .      "Retrieve info for 1+ blogs")
      (tumblr-posts .          "Retrieve all posts for 1+ blog")
      (tumblr-tag .            "Retrieve posts matching 1+ tags")))

  ;; Command line arguments supported by each task
  (define tumblr-task-options
    '((tumblr-auth .           "auth")
      (tumblr-blog-info .      "query*")
      (tumblr-posts .          "query* count*")
      (tumblr-tag .            "query* count*")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Oauth Procedures
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; Some calls to Tumblr's apis must be signed with oauth. The
  ;; following procedures provide the "service" and "token-credential"
  ;; required by with-oauth

  ;; This returns a structure for use as a "service" when calling
  ;; with-oauth 
  (define (tumblr-service #!key consumer-key consumer-secret)
    (let ((tumblr-provider
	   (make-oauth-service-provider
	    protocol-version: '1.0a
	    credential-request-url: "https://www.tumblr.com/oauth/request_token"
	    owner-auth-url: "https://www.tumblr.com/oauth/authorize"
	    token-request-url: "https://www.tumblr.com/oauth/access_token"
	    signature-method: 'hmac-sha1)))
      (make-oauth-service
       service: tumblr-provider
       client-credential: (make-oauth-credential consumer-key
						 consumer-secret))))
  
  ;; This returns a "token-credential" structure for use with
  ;; with-oauth 
  (define (tumblr-token-credential #!key
				   access-token
				   access-token-secret)
    (make-oauth-credential access-token access-token-secret))


  ;; This returns the user's tumblr credentials, if available. If the
  ;; user has not provided this information previously, an error is
  ;; reported 
  (define (tumblr-auth cred-path)
    (let ((cred-file (if cred-path cred-path (tumblr-cred-file))))
      (if (file-exists? cred-file)
	  ;; Return tumblr credential information
	  (with-input-from-file cred-file read)
	  ;; Else the user needs set up their credentials
	  (begin
	    (display "Authenticate before using Tumblr.\nRun --> 'massmine --task=tumblr-auth'\n"
		     (current-error-port))
	    (exit 1)))))

  ;; Helper function: get confirmation from user
  (define (yes-or-no? msg #!key default)
    (print (string-append msg " (yes/no)"))
    (let ((resp (string-trim-both (read-line))))
      (cond ((string-ci=? "yes" resp) #t)
	    (else #f))))

  ;; This walks the user through setting up their Tumblr credentials
  ;; for MassMine
  (define c-key (make-parameter ""))
  (define c-secret (make-parameter ""))
  (define a-token (make-parameter ""))
  (define a-secret (make-parameter ""))
  (define (tumblr-setup-auth cred-file)
    (let ((cred-path (if cred-file cred-file (tumblr-cred-file))))
      (print "Would you like to setup your Tumblr credentials?")
      (print "Warning: continuing will over-write any previous credentials")
      (if (yes-or-no? "Continue?" #:default "No" #:abort #f)
	  ;; Walk the user through setting up their credentials
	  (begin
	    (print "Please visit https://www.tumblr.com/oauth/apps to ")
	    (print "collect your consumer key and secret key. Next, ")
	    (print "visit https://api.tumblr.com/console/calls/user/info")
	    (print "to generate your token and token secret. Supply each")
	    (print "below")
	    (display "Consumer key: ")
	    (c-key (string-trim-both (read-line)))
	    (display "Consumer secret: ")
	    (c-secret (string-trim-both (read-line)))
	    (display "Token: ")
	    (a-token (string-trim-both (read-line)))
	    (display "Token secret: ")
	    (a-secret (string-trim-both (read-line)))

	    ;; Prepare a proper alist and write to disk
	    (with-output-to-file cred-path
	      (lambda ()
		(write `((consumer-key . ,(c-key))
			 (consumer-secret . ,(c-secret))
			 (access-token . ,(a-token))
			 (access-token-secret . ,(a-secret))))))
	    (print "\nAuthentication setup finished!"))
	  (print "Stopping!"))))

  ;; Hostname helper procedure. blog names (used in other procedures
  ;; below) should follow Tumblr's "standard" or "custom" hostname
  ;; format (e.g., greentype.tumblr.com or www.davidslog.com ,
  ;; respectively). The "standard" format is more common, and we don't
  ;; want users to have to include the ".tumblr.com" suffix every
  ;; time. This procedure detects if a "custom" hostname has been
  ;; passed. If not, the suffix ".tumblr.com" is appended and returned
  (define (format-tumblr-hostname blog-name)
    (if (string-contains blog-name ".")
	blog-name
	(string-append blog-name ".tumblr.com")))
  
  ;; Returns info about 1 or more blogs. Multiple blogs can be passed
  ;; by separating with commas. api-key is (car (alist-ref
  ;; 'client-credential tumblr-app))
  (define (tumblr-blog-info blog api-key)
    (let ((blog-names (map string-trim-both (string-split blog ","))))
      (for-each (lambda (curr-blog)
		  (let ((result (call-with-input-request
				 (string-append "https://api.tumblr.com/v2/blog/"
						(format-tumblr-hostname curr-blog)
						"/info?api_key="
						api-key)
				 #f
				 read-json)))
		    (write-json (alist-ref 'blog (alist-ref 'response result)))
		    (newline)))
		blog-names)))

  ;; Returns all posts (in plain text format) for a given blog (or
  ;; comma-separated list of blogs)
  (define (tumblr-posts blog max-posts api-key)
    (let ((blog-names (map string-trim-both (string-split blog ","))))
      (for-each (lambda (curr-blog)
		  (let loop ((offset 0))
		    (let* ((result (call-with-input-request
				    (string-append "https://api.tumblr.com/v2/blog/"
						   (format-tumblr-hostname curr-blog)
						   "/posts/?api_key="
						   api-key
						   "&filter=text&offset="
						   (number->string offset))
				    #f
				    read-json))
			   (all-posts (alist-ref 'posts (alist-ref 'response result)))
			   (num-posts (vector-length all-posts)))
		      ;; Write each post
		      (for-each (lambda (curr-post)
				  (write-json curr-post)
				  (newline))
				(vector->list all-posts))
		      ;; The api endpoint returns 20 posts max each
		      ;; pull. The field respons.total_posts reveals
		      ;; how many are available. If we haven't reached
		      ;; that number, we make another pull
		      (unless (or (>= (+ offset num-posts) (alist-ref 'total_posts (alist-ref 'response result)))
				  (>= (+ offset num-posts) max-posts))
			(loop (+ offset num-posts))))))
		blog-names)))

  ;; Returns all posts (in plain text format) that match a given tag,
  ;; up to max-posts number of posts
  (define (tumblr-tag tag max-posts api-key)
    (let ((tag-names (map string-trim-both (string-split tag ","))))
      (for-each (lambda (curr-tag)
		  (let loop ((before-timestamp (current-seconds))
			     (total-count 0))
		    (let* ((result (call-with-input-request
				    (string-append "https://api.tumblr.com/v2/tagged?tag="
						   curr-tag
						   "&api_key="
						   api-key
						   "&filter=text&before="
						   (number->string before-timestamp))
				    #f
				    read-json))
			   (num-posts (vector-length (alist-ref 'response result)))
			   (next-timestamp (alist-ref
					    'featured_timestamp
					    (vector-ref (alist-ref
							 'response result) (- num-posts 1)))))
		      ;; Write each post
		      (for-each (lambda (curr-post)
				  (write-json curr-post)
				  (newline))
				(vector->list (alist-ref 'response result)))
		      ;; The api endpoint returns 20 posts max each
		      ;; pull. If we haven't reached max-posts, pull again
		      (unless (>= (+ total-count num-posts) max-posts)
			(loop next-timestamp (+ total-count num-posts))))))
		tag-names)))


  ) ;; end of module massmine-tumblr

;; tumblr.scm ends here
