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
;; Instructions: See www.massmine.org for complete documentation and
;; instructions for how to use massmine

(module massmine-google *

  (import scheme chicken)
  (use extras)
  (use http-client medea)

  ;; user-agent header used in http-header of all calls
  (client-software '(("MassMine" "0.9.5 (2015-08-28)" #f)))

  ;; Available tasks and brief descriptions
  (define google-task-descriptions
    '((google-trends . "Trending keywords on Google")))

  ;; Command line arguments supported by each task
  (define google-task-options
    '((google-trends . "<none>")))


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
  (define (get-google-trends)
    (call-with-input-request
     "http://hawttrends.appspot.com/api/terms/"
     #f read-json))

  ;; Restructure and write the data as JSON
  (define (google-trends)
    (let ((trend-data (get-google-trends)))
      (for-each (lambda (country)
  		  (let ((cname (gcode->location (car country))))
		    (for-each (lambda (trend)
				(write-json `((location . ,cname) (query . ,trend)))
				(newline))
			      (vector->list (cdr country)))))
  		trend-data)))

  ) ;; end of module massmine-google

;; google.scm ends here
