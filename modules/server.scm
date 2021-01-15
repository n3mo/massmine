;; ##################################################################
;;
;; MassMine: Your Access To Data
;; Copyright (C) 2014-2020 Nicholas M. Van Horn & Aaron Beveridge
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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Server/Client Documentation
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Once the massmine server is running, you can send commands over
;;; tcp to the specified port. massmine's behavior is:

;;; (1) Listen on the specified port for JSON strings
;;; (2) Convert JSON strings into internal commands
;;; (3) Run the internal command
;;; (4) Write the results, in line-oriented JSON, to the specified
;;;     port
;;; (5) Write the string "0" (indicating success) or "1" (indicating
;;;     failure) to the specified port to signal the end of the data
;;;     stream. If "1" is returned, the previous JSON array will
;;;     contain an element called "error" which contains the error
;;;     information 
;;; (6) Repeat until the "stop" task is requested, at which time
;;;     the massmine process itself will terminate (after closing
;;;     ports, etc.)

;;; Clients set to read from the massmine port should be prepared for
;;; potentially very long timeouts (e.g., 15 minutes) in the middle of
;;; a data transmission. This is because massmine will pause to comply
;;; with rate limits set by upstream data sources. Clients should
;;; check for the "end of data stream" signals by monitoring for the
;;; "0" or "1" signals issued in step 5 above. After the signal has
;;; been caught, additional commands can be issued to massmine.

;;; Commands should be sent as JSON structures containing name:value
;;; pairs that match the command line arguments issued to
;;; massmine. For example, the command:

;;; massmine --task=twitter-search --query=politics --count=10

;;; Could be called in "server mode" by first starting massmine:
;;; massmine --server=4242

;;; and then passing the following JSON structure to port 4242:
;;; {"task":"twitter-search","query":"politics","count":10}

;;; massmine can be closed with the special command:
;;; {"task":"stop"}

;;; ------- Minimal working example

;;; To test MassMine's server mode, a quick solution entails the
;;; netcat command line tool:

;;; Step 1: start massmine: massmine --server=4242
;;; Step 2: start netcat: netcat localhost 4242
;;; Step 3: type/paste json commands and press enter
;;; Step 4: massmine will return results
;;; Step 5: massmine returns "0" or "1" when finished
;;; Step 6: Repeat steps 3-5 for each test command
;;; Step 7: End with json command {"task":"stop"}

;;; ------- Minimal working example <END>

(import (chicken io) (chicken port))
(import tcp6 medea)

;;; Server port parameters. There are no timeouts. massmine will
;;; listen until you send the command {"task":"stop"} or Ctrl+c
(tcp-read-timeout #f)
(tcp-write-timeout #f)
(tcp-connect-timeout #f)
(tcp-accept-timeout #f)

;;; Task manager
(define (server-task cmd)
  ;; Set parameters
  (task (alist-ref 'task cmd))
  (keywords (alist-ref 'query cmd eqv? ""))
  (locations (alist-ref 'geo cmd eqv? ""))
  (language (alist-ref 'lang cmd eqv? ""))
  (user-info (alist-ref 'user cmd eqv? ""))
  (if (alist-ref 'count cmd)
      (max-tweets (alist-ref 'count cmd))
      (max-tweets 999999999))
  ;; Double-check data type in case user supplies numeric value as
  ;; string
  (if (not (number? (max-tweets))) (max-tweets (string->number (max-tweets))))
  (if (alist-ref 'dur cmd)
      (global-max-seconds (date-string->seconds (alist-ref 'dur cmd))))
  (date (alist-ref 'date cmd eqv? "2100-01-01"))
  (config-file (alist-ref 'config cmd)))

;;; This is called by massmine core and does the heavy lifting
(define (massmine-server #!key [port 4242])
  ;; Prepare massmine server by listening to port
  (define listener (tcp-listen port))
  (define-values (i o) (tcp-accept listener))
  ;; Start server
  (let listen ([cmd (read-line i)])
    (handle-exceptions
	exn
	;; Error handling
	(begin (write-json
		`((error . ,((condition-property-accessor
			      'exn 'message) exn))) o)
	       (newline o)
	       (write-line "1" o)
	       (listen (read-line i)))
      ;; Run unless error
      (if (eof-object? cmd)
	  (listen (read-line i))
	  (let ((json-cmd (read-json cmd)))
	    (server-task json-cmd)
	    (if (string-ci=? (task) "stop")
		(begin
		  (write-line "Stopping Server...\n")
		  (close-input-port i)
		  (close-output-port o))
		(begin
		  (with-output-to-port o
		    (lambda ()
		      (task-dispatch (task))
		      (write-line "0" o)
		      (listen (read-line i)))))))))))

;;; End of file server.scm
