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
;; x.x.x 2015-05-01 NVH: Initial chicken version
;;
;; Instructions: See www.massmine.org for complete documentation and
;; instructions for how to use massmine

(module massmine-wikipedia *

  (import scheme)
  (import (chicken base) (chicken io) (chicken string) (chicken irregex))
  (import srfi-1 srfi-13)
  (import openssl)
  (import rest-bind uri-common medea http-client)

  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "1.3.0 (2021-01-15)" #f)))

  ;; Lots of web services, including Twitter, don't accept ';' separated
  ;; query strings so use '&' for encoding by default but support both
  ;; '&' and ';' for decoding.
  (form-urlencoded-separator "&;")

  ;; Wikipedia URL. Different languages begin with different URLs. For
  ;; example, English Wikipedia pages begin with en.wikipedia.org
  ;; whereas Japanses Wikipedia pages begin with
  ;; ja.wikipedia.org. Users that set the --lang option at runtime can
  ;; specify the prefix LANG in the URL LANG.wikipedia.org
  (define wiki-language (make-parameter "en"))
  (define (wikipedia-url)
    (string-append "https://" (wiki-language) ".wikipedia.org"))

  ;; Available tasks and brief descriptions
  (define wikipedia-task-descriptions
    '((wikipedia-page-links . "Retrieve links embedded in a given wiki page")
      (wikipedia-search . "Search wikipedia by keyword(s)")
      (wikipedia-text   . "Retrieve full text of wiki page")
      (wikipedia-trends . "Retrieve 1000 most viewed pages for a month or day")
      (wikipedia-views  . "Retrieve daily page views for a range of dates")))

  ;; Command line arguments supported by each task
  (define wikipedia-task-options
    '((wikipedia-page-links . "query* lang")
      (wikipedia-search . "query* lang")
      (wikipedia-text   . "query* lang*")
      (wikipedia-trends . "date* (as YYYY-MM or YYYY-MM-DD)")
      (wikipedia-views  . "query* date* (as YYYY-MM-DD:YYYY-MM-DD or \n\t\t\t\t\tYYYY-MM-DD-HH:YYYY-MM-DD-HH)")))

  ;; HELPER PROCEDURES -----------------------------------------------

  ;; printf-like procedure for padding numbers (in strings) so that
  ;; they're always len numbers long. Example (left-pad 4 2) --> "04"
  (define (left-pad number len)
  (let* ((nstr (number->string number))
         (diff (- len (string-length nstr)))
         (pads (if (> diff 0) (make-string diff #\0) "")))
    (string-append pads nstr)))

  ;; Date range finder. Returns number of months between two dates
  ;; ("2014-01-01" "2015-04-01") --> 16
  (define (total-months date)
    (let ((mindate (map string->number (string-split (car date) "-")))
	  (maxdate (map string->number (string-split (cadr date) "-"))))
      (let ((year-month(take (map - maxdate mindate) 2)))
	(+ (* 12 (car year-month)) (cadr year-month) 1))))

  ;; Turns a massmine date string spanning multiple dates (e.g.,
  ;; "2014-01-01:2015-01-01") into a list of dates suitable for use
  ;; with wikipedia-views (format YYYYMM)
  (define (date-span->months date)
    (let* ((drange (string-split date ":"))
	   (mindate (map string->number (string-split (car drange) "-")))
	   (minyear (car mindate))
	   (minmonth (cadr mindate))
	   (months (total-months drange)))
      (map
       (lambda (x)
	 (let* ((num-years (inexact->exact (floor (/ x 12))))
	       (month-num (- x (* 12 num-years))))
	   (string-append (number->string (+ minyear num-years))
			  (left-pad (+ minmonth month-num) 2))))
       (iota months))))

  ;; URI encoder. Same as uri-encode-string from uri-common, but
  ;; converts spaces to underscores first, according to the
  ;; requirements of wikimedia's API
  (define (wiki-uri-encode str)
    (uri-encode-string (irregex-replace/all (irregex-quote " ") str "_")))
  
  ;; EXAMPLES ------------------------------------------------------------

  ;; Searching wikipedia. Here's an example GET method for searching
  ;; for up to 100 (the max) pages with the keyword "vegan" in the
  ;; title
  ;; (call-with-input-request
  ;;  "https://en.wikipedia.org/w/api.php?action=opensearch&search=vegan&limit=100&format=json"
  ;;  #f read-json)

  ;; Use sroffset to page through results. Each json return from a
  ;; search using srsearch (https://www.mediawiki.org/wiki/API:Search)
  ;; includes a (continue (sroffset . n)... field, where n will be
  ;; equal to the srlimit (the requested number of search results) in
  ;; your original call. srlimit max is 50 for users (but 500 for
  ;; bots... however you make that happen). Use the sroffset value to
  ;; return the next page of 50 results
  ;; (define tmp (call-with-input-request "https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=vegan&srlimit=50&sroffset=50&format=json" #f read-json))

  ;; Grabbing the plain text of a specific page
  ;; (call-with-input-request
  ;;  "https://en.wikipedia.org/w/api.php?action=query&prop=extracts&explaintext&titles=Veganism&format=json"
  ;;  #f read-json)

  ;; API METHODS -------------------------------------------------------

  ;; Search API method (see https://www.mediawiki.org/wiki/API:Search)
  (define-method (wiki-search #!key srsearch srnamespace srwhat srinfo
			      srprop srredirects sroffset srlimit
			      srinterwiki srbackend)
    (string-append (wikipedia-url) "/w/api.php?action=query&list=search&utf8&continue=&format=json")
    #f read-line #f)

  ;; Fetch plain text of page by title
  ;; (define-method (wiki-page-text #!key titles)
  ;;   (string-append (wikipedia-url) "/w/api.php?action=query&prop=extracts&explaintext&utf8&format=json")
  ;;   #f read-line #f)
  (define (wiki-page-text #!key titles)
    (let ((url (string-append (wikipedia-url)
			      "/w/api.php?action=query&prop=extracts&explaintext&utf8&format=json&titles="
			      titles)))
      (call-with-input-request url #f read-line)))

  ;; Daily page view statistics. title should be properly URI encoded
  ;; using wiki-uri-encode. Dates should be in the format
  ;; YYYYMMDDHH or YYYYMMDD
  (define (wiki-page-views #!key title from-date to-date)
    (let ((url (string-append
  		"https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/"
  		title "/daily/"
  		from-date "/"
		to-date)))
      (call-with-input-request url #f read-json)))


  ;; Extract links from a wiki page
  (define-method (wiki-page-links #!key titles plnamespace pllimit
				  plcontinue)
    (string-append (wikipedia-url) "/w/api.php?action=query&prop=links&format=json")
    #f read-line #f)

  ;; Find the 1000 most viewed articles on wikipedia for a given month
  ;; or day
  (define (wiki-trends #!key year month day)
    (let ((url (string-append
		"https://wikimedia.org/api/rest_v1/metrics/pageviews/top/en.wikipedia/all-access/"
		year "/"
		month "/"
		day)))
      (call-with-input-request url #f read-json)))

  ;; WIKIPEDIA TASKS -------------------------------------------------

  ;; Returns the full text of a wikipedia page given its title. If
  ;; pagetitle is a comma-separated list of wiki pages, each page's
  ;; data is returned.
  (define (wikipedia-text pagetitle lang)
    (if lang (wiki-language lang))
    (if (string-contains pagetitle ",")
	(let ((titles (string-split pagetitle ",")))
	  (for-each
	   (lambda (curr-title)
	     (let* ((raw-return
		     (read-json (wiki-page-text #:titles (string-trim-both curr-title))))
		    (page-data (car (alist-ref 'pages (alist-ref 'query (cdr raw-return))))))
	       (begin
		 (write-json (filter pair? page-data))
		 (newline))))
	   titles))
	(let* ((raw-return (read-json (wiki-page-text #:titles pagetitle)))
	       (page-data (car (alist-ref 'pages (alist-ref 'query (cdr raw-return))))))
	  (begin
	    (write-json (filter pair? page-data))
	    (newline)))))

  ;; Search wikipedia. This returns either all search results (paging
  ;; through results as necessary) matching a query, or --count many
  ;; matches
  (define (wikipedia-search query lang)
    (if lang (wiki-language lang))
    (let loop ((kwords query) (offset 0))
      (let* ((results
	      (read-json (wiki-search #:srsearch kwords #:srlimit 50 #:sroffset offset)))
	     (continue? (alist-ref 'continue results)))
	(for-each (lambda (x) (write-json x) (newline))
		  (vector->list (alist-ref 'search (alist-ref 'query results))))
	(if continue? (loop kwords (alist-ref 'sroffset continue?))))))

  ;; Retrieve links embedded in a given wiki page
  (define (wikipedia-page-links query lang)
    (if lang (wiki-language lang))
    (let loop ((page-title query) (offset "") (first? #t))
      (let* ((results
	      (if first?
		  (read-json (wiki-page-links #:titles page-title
					      #:pllimit 500))
		  (read-json (wiki-page-links #:titles page-title
					      #:pllimit 500
					      #:plcontinue offset))))
	     (continue? (alist-ref 'continue results)))
	(for-each (lambda (x) (write-json (cons `(source . ,page-title) x)) (newline))
		  (vector->list
		   (alist-ref 'links (cdr (car (alist-ref 'pages (alist-ref 'query results)))))))
	(if continue?
	    (loop page-title (alist-ref 'plcontinue (alist-ref 'continue results)) #f)))))

  ;; Retrieve wikipedia page views for each day of a given range of
  ;; dates (specified by YYYY-MM-DD:YYYY-MM-DD or
  ;; YYYY-MM-DD-HH:YYYY-MM-DD-HH). Title should be a valid wikipedia
  ;; article name (although spaces will be handled for the user if
  ;; they include them)
  (define (wikipedia-views title date)
    (let ((date-range (string-split (irregex-replace/all "-" date "") ":")))
      (let ((result (wiki-page-views
		     #:title (wiki-uri-encode title)
		     #:from-date (first date-range)
		     #:to-date (second date-range))))
	(for-each
	 (lambda (day)
	   (write-json `((title . ,(alist-ref 'article day))
			 (date . ,(alist-ref 'timestamp day))
			 (views . ,(alist-ref 'views day))))
	   (newline))
	 (vector->list (alist-ref 'items result))))))

  ;; Retrieve the 1000 most trending articles on wikipedia for a given
  ;; month or day. date, a string, should be either a month as YYYY-MM
  ;; or a day as YYYY-MM-DD
  (define (wikipedia-trends date)
    ;; Helper function for date output
    (define (date-me year month day)
      (if (string=? day "all-days")
	  (string-append year "-" month)
	  (string-append year "-" month "-" day)))
    ;; This gets things done
    (let* ((tmp (string-split date "-"))
	   (year (first tmp))
	   (month (second tmp))
	   (day (if (= (length tmp) 3)
		    (third tmp)
		    "all-days"))
	   (out-date (date-me year month day))
	   (result (wiki-trends #:year year #:month month #:day day)))
      (for-each
       (lambda (entry)
	 (write-json `((date . ,out-date)
		       (title . ,(alist-ref 'article entry))
		       (rank . ,(alist-ref 'rank entry))
		       (views . ,(alist-ref 'views entry))))
	 (newline))
       (vector->list
	(alist-ref 'articles (first (vector->list (alist-ref 'items result))))))))

  ) ;; end of module massmine-wikipedia

;; wikipedia.scm ends here
