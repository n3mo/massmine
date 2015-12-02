;; ##################################################################
;;
;; MassMine: Your Access To Data
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
;; Instructions: See www.massmine.org for complete documentation and
;; instructions for how to use massmine

(module massmine-google *

  (import scheme chicken)
  (use extras srfi-13 data-structures)
  (use http-client medea)

  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "0.10.0 (2015-10-09)" #f)))

  ;; Available tasks and brief descriptions
  (define google-task-descriptions
    '((google-country-trends . "Trending phrases by country")
      (google-trends . "Trending US search phrases")))

  ;; Command line arguments supported by each task
  (define google-task-options
    '((google-country-trends . "<none>")
      (google-trends . "date* (YYYY-MM-DD)")))


  ;; HELPER PROCEDURES -----------------------------------------------

  ;; Google API data returned with coded country names. Convert code
  ;; to string name
  (define (gcode->location gcode)
    (cond
     [(equal? gcode '|30|) "Argentina"]
     [(equal? gcode '|8|) "Australia"]
     [(equal? gcode '|44|) "Austria"]
     [(equal? gcode '|41|) "Belgium"]
     [(equal? gcode '|18|) "Brazil"]
     [(equal? gcode '|13|) "Canada"]
     [(equal? gcode '|38|) "Chile"]
     [(equal? gcode '|32|) "Colombia"]
     [(equal? gcode '|43|) "Czech Republic"]
     [(equal? gcode '|49|) "Denmark"]
     [(equal? gcode '|29|) "Egypt"]
     [(equal? gcode '|50|) "Finland"]
     [(equal? gcode '|16|) "France"]
     [(equal? gcode '|15|) "Germany"]
     [(equal? gcode '|48|) "Greece"]
     [(equal? gcode '|10|) "Hong Kong"]
     [(equal? gcode '|45|) "Hungary"]
     [(equal? gcode '|3|) "India"]
     [(equal? gcode '|19|) "Indonesia"]
     [(equal? gcode '|6|) "Israel"]
     [(equal? gcode '|27|) "Italy"]
     [(equal? gcode '|4|) "Japan"]
     [(equal? gcode '|37|) "Kenya"]
     [(equal? gcode '|34|) "Malaysia"]
     [(equal? gcode '|21|) "Mexico"]
     [(equal? gcode '|17|) "Netherlands"]
     [(equal? gcode '|52|) "Nigeria"]
     [(equal? gcode '|51|) "Norway"]
     [(equal? gcode '|25|) "Philippines"]
     [(equal? gcode '|31|) "Poland"]
     [(equal? gcode '|47|) "Portugal"]
     [(equal? gcode '|39|) "Romania"]
     [(equal? gcode '|14|) "Russia"]
     [(equal? gcode '|36|) "Saudi Arabia"]
     [(equal? gcode '|5|) "Singapore"]
     [(equal? gcode '|40|) "South Africa"]
     [(equal? gcode '|23|) "South Korea"]
     [(equal? gcode '|26|) "Spain"]
     [(equal? gcode '|42|) "Sweden"]
     [(equal? gcode '|46|) "Switzerland"]
     [(equal? gcode '|12|) "Taiwan"]
     [(equal? gcode '|33|) "Thailand"]
     [(equal? gcode '|24|) "Turkey"]
     [(equal? gcode '|35|) "Ukraine"]
     [(equal? gcode '|9|) "United Kingdom"]
     [(equal? gcode '|1|) "United States"]
     [(equal? gcode '|28|) "Vietnam"]))

  ;; Fetch the latest data. Top 20 search phrase for each of 47 countries
  (define (get-google-country-trends)
    (call-with-input-request
     "http://hawttrends.appspot.com/api/terms/"
     #f read-json))

  ;; Restructure and write the data as JSON
  (define (google-country-trends)
    (let ((trend-data (get-google-country-trends)))
      (for-each (lambda (country)
  		  (let ((cname (gcode->location (car country))))
		    (for-each (lambda (trend)
				(write-json `((location . ,cname) (query . ,trend)))
				(newline))
			      (vector->list (cdr country)))))
  		trend-data)))

  ;; Strategies gleaned from http://techslides.com/hacking-the-google-trends-api

  ;; Get the top-20 trends for the US
  (define (get-google-trends date-stamp)
    ;; Set browser-like user-agent
    (client-software '(("Mozilla/5.0" "(Windows NT 6.2; Win64; x64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1" #f)))
    (call-with-input-request
      "http://www.google.com/trends/hottrends/hotItems"
      `((ajax . "1")
	(htd . ,(string-delete (lambda (x) (equal? x #\-)) date-stamp))
	(pn . "p1")
	(htv . "l"))
      read-json))

  ;; Provides richer information about the current top-20 trends in
  ;; the US
  (define (google-trends date-stamp)
    (let* ((trend-data (get-google-trends date-stamp))
	   (trend-list (vector->list
			(alist-ref
			 'trendsList
			 (car (vector->list (alist-ref 'trendsByDateList trend-data)))))))
      (for-each (lambda (trend) (write-json trend) (newline))
		trend-list)))

  ) ;; end of module massmine-google

;; google.scm ends here
