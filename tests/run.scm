
(import test)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               MassMine Core Tests
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "MassMine Core")

(test-assert "Examples" (string? massmine-examples))
(test-assert "Arg Options" (list? opts))
(test-assert "String prefix checking" (s-starts-with? "1." "1. Test"))

(test "Date handling" 0
      (date-string->seconds
       (date->string
	(seconds->date (current-seconds)) "~Y-~m-~d ~H:~M:~S")))

(test-end "MassMine Core")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Google Tests
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "Google Module")

;;; Helper functions
(test-assert "Google task descriptions" (list? google-task-descriptions))
(test-assert "Google task options" (list? google-task-options))
(test "Country Code Identification"
      "Romania" (gcode->location '|39|))

;;; Tasks
(test-assert "Google country trends"
  (string? (with-output-to-string (lambda () (google-country-trends)))))
;; (test-assert "Google trends"
;;   (string? (with-output-to-string (lambda () (google-trends "2016-01-01")))))

(test-end "Google Module")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Tumblr Tests
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "Tumblr Module")

(test-assert "Tumblr task descriptions" (list? tumblr-task-descriptions))
(test-assert "Tumblr task options" (list? tumblr-task-options))
(test "Tumblr hostname parsing [1/2]"
      "www.example.com"
      (format-tumblr-hostname "www.example.com"))
(test "Tumblr hostname parsing [2/2]"
      "example.tumblr.com"
      (format-tumblr-hostname "example"))

(test-end "Tumblr Module")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Twitter Tests
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "Twitter Module")

(test-assert "Twitter task descriptions" (list? twitter-task-descriptions))
(test-assert "Twitter task options" (list? twitter-task-options))
(test-assert "Search rate limit" (list? search-rate-limit))
(test-assert "Trends rate limit" (list? trends-rate-limit))
(test-assert "Timeline rate limit" (list? timeline-rate-limit))
(test-assert "Friends rate limit" (list? friends-rate-limit))
(test-assert "Followers rate limit" (list? followers-rate-limit))

(test-end "Twitter Module")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Web tests
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "Web Module")

(test-assert "Web task descriptions" (list? web-task-descriptions))
(test-assert "Web task options" (list? web-task-options))
(test-assert "Web url to string capture"
  (string? (url-content->string "http://www.google.com")))
(test-assert "Web text extraction"
  (string? (with-output-to-string (lambda () (web-text "http://www.massmine.org")))))

(test-end "Web Module")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Wikipedia Tests
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "Wikipedia Module")

(test-assert "Wikipedia task descriptions" (list? wikipedia-task-descriptions))
(test-assert "Wikipedia task options" (list? wikipedia-task-options))
(test "Wikipedia url builder"
      "https://en.wikipedia.org"
      (wikipedia-url))
(test "Number padding"
      "04"
      (left-pad 4 2))
(test "Month counter"
      16
      (total-months '("2014-01-01" "2015-04-01")))
(test "Date span aggregator"
      13
      (length (date-span->months "2014-01-01:2015-01-01")))

;;; Tasks
(test-assert "Wikipedia page links"
  (string? (with-output-to-string
	     (lambda () (wikipedia-page-links "marmot" "en")))))
(test-assert "Wikipedia search"
  (string? (with-output-to-string
	     (lambda () (wikipedia-search "marmot" "en")))))
(test-assert "Wikipedia text"
  (string? (with-output-to-string
	     (lambda () (wikipedia-text "Veganism" "en")))))
(test-assert "Wikipedia trends"
  (string? (with-output-to-string
	     (lambda () (wikipedia-trends "2018-04")))))
(test-assert "Wikipedia views"
  (string? (with-output-to-string
	     (lambda () (wikipedia-views "Veganism"
					 "2018-01-01:2018-01-02")))))

(test-end "Wikipedia Module")

;;; End with exit
(test-exit)
