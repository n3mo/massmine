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

(module massmine-wikipedia *

  (import scheme chicken)
  (use extras data-structures srfi-1 srfi-13)
  (use rest-bind uri-common medea http-client)

  ;; Lots of web services, including Twitter, don't accept ';' separated
  ;; query strings so use '&' for encoding by default but support both
  ;; '&' and ';' for decoding.
  (form-urlencoded-separator "&;")

  ;; Available tasks and brief descriptions
  (define wikipedia-task-descriptions
    '((wikipedia-search . "Search wikipedia by keyword(s)")
      (wikipedia-text   . "Retrieve full text of wiki page")))


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
    "https://en.wikipedia.org/w/api.php?action=query&list=search&utf8&continue=&format=json"
    #f read-line #f)

  ;; Fetch plain text of page by title
  (define-method (wiki-page-text #!key titles)
    "https://en.wikipedia.org/w/api.php?action=query&prop=extracts&explaintext&utf8&format=json"
    #f read-line #f)


  ;; WIKIPEDIA TASKS -------------------------------------------------

  ;; Returns the full text of a wikipedia page given its title. If
  ;; pagetitle is a comma-separated list of wiki pages, each page's
  ;; data is returned.
  (define (wikipedia-text pagetitle)
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
  (define (wikipedia-search query)
    (let loop ((kwords query) (offset 0))
      (let* ((results
	      (read-json (wiki-search #:srsearch kwords #:srlimit 50 #:sroffset offset)))
	     (continue? (alist-ref 'continue results)))
	(for-each (lambda (x) (write-json x) (newline))
		  (vector->list (alist-ref 'search (alist-ref 'query results))))
	(if continue? (loop kwords (alist-ref 'sroffset continue?))))))

  ) ;; end of module massmine-wikipedia

;; wikipedia.scm ends here
