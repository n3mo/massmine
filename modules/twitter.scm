(module massmine-twitter *

  (import scheme chicken)
  (use extras irregex data-structures)
  (use openssl oauth-client uri-common rest-bind medea clucker)

  ;; (load "./modules/clucker.scm")
  ;; (import clucker)

  ;; The use of Twitter's API requires OAuth 1.0a for authenticated
  ;; connections. The oauth egg handles this. Before using any of the
  ;; functionality of this egg, you must set up and authenticate your
  ;; credentials with Twitter's services. This can be done as follows

  ;; Set up the service provider
  (define twitter (make-oauth-service-provider
		   protocol-version: '1.0a
		   credential-request-url: "https://api.twitter.com/oauth/request_token"
		   owner-auth-url: "https://api.twitter.com/oauth/authorize"
		   token-request-url: "https://api.twitter.com/oauth/access_token"
		   signature-method: 'hmac-sha1))

  ;; Set up the application to include your secret tokens (available at
  ;; apps.twitter.com)
  (define twitter-app
    (make-oauth-service
     service: twitter
     client-credential: (make-oauth-credential
			 "yTaD9IZxX9VBozSE6gp7WA"
			 "3Mrn7lKd5fKpDLiH4y5W81totNtquoQdEdOAtHUXBU")))

  ;; Acquire a new credentials ------------------------------------
  ;; (define x (acquire-temporary-credential twitter-app))
  ;; (uri->string (authorize-resource-owner twitter-app x))
  ;; Visit the supplied URL in browser, allow app and retrive verifier
  ;; (define me (acquire-token-credential twitter-app x <VERIFIER>))
  ;; end of code for acquiring new credentials -------------------

  ;; Use an existing, already confirmed set of credentials
  (define me (make-oauth-credential
	      "1961116760-b3tyEWzZAFoKElzjnYyBf2EPRt9uKDeo7GQTqyQ"
	      "bYpDF5lqv0ck1WNyjcQp3pRV73oD4rAgoHwzecEgePU"))

  ;; This is the call to twitter's streaming api.
  (define (fetch-data pattern geo-locations)
    (handle-exceptions exn
	;; Exception handler does nothing but suppress the inevitable
	;; error caused but terminating the connection manually
	#t
      ;; This does the real work
      (with-oauth twitter-app me
		  (lambda ()
		    (statuses-filter-method #:delimited "length"
					    #:track pattern
					    #:locations geo-locations)))))
  ) ;; end of module massmine-twitter

;; twitter.scm ends here
