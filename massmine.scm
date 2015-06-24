#! /usr/local/bin/csi -s
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

;; WARNING @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; You are working in the chicken branch of this git repo

;; (declare (uses massmine-twitter))

;; Extensions. We need to import clucker here just so we can set! the
;; global variables used to sever the https connection with Twitter's
;; streaming API (which are defined in clucker)
(require-extension args clucker openssl posix oauth-client openssl)

(include "./modules/twitter")
(import massmine-twitter)
;;(import clucker)

;; Master parameter alist
(define P '((mm-cred-path . "~/.config/massmine")))

;; Current version of software
;; mm_version = 'x.x.x (2015-05-01)'
(define mm-version "x.x.x (2015-06-22)")

;;; The following list contains all defined command line options
;;; available to the user. For example, (h help) makes all of the
;;; following equivalent options available at runtime: -h, -help, --h,
;;; --help. These are used by the "args" egg.
(define opts
  (list (args:make-option (h help)    #:none "Help information"
			  (usage))
	(args:make-option (v version)  #:none "Version information"
			  (print-version))
	(args:make-option (output)  (required: "FILE")  "Write to file"
			  (set! output-to-file? #t))
	(args:make-option (task)  (required: "TASK") "Task name"
			  (set! task #f))
	(args:make-option (pattern)  (required: "KEYWORDS") "Keyword(s)"
			  (set! keywords #f))
	(args:make-option (count)  (required: "NUM") "Number of records"
			  (set! max-tweets #f))
	(args:make-option (time)  (required: "SECOND") "Duration"
			  (set! global-max-seconds #f))
	(args:make-option (geo)  (required: "COORDINATE") "Location"
			  (set! locations #f))
	(args:make-option (lang)  (required: "LANG") "Language"
			  (set! locations #f))
	(args:make-option (user)  (required: "NAME") "Screen name"
			  (set! screen-name #f))
	(args:make-option (no-splash)  #:none "Inhibit splash screen"
			  (set! do-splash? #f))
	))

;;; This procedure is called whenever the user specifies the help
;;; option at runtime OR whenever an unexpected command line option or
;;; operand is passed to this script.
(define (usage)
 (with-output-to-port (current-error-port)
   (lambda ()
     (print "Usage: massmine ... [options...]")
     (newline)
     (print (args:usage opts))
     (print "Retrieve and store data from web sources")
     (newline)
     (print "Report bugs to nemo1211 at gmail.")))
 (exit 1))

;; Prints the current version of massmine, and useful info
(define (print-version)
  (print "massmine " mm-version)
  (print "https://github.com/n3mo/massmine")
  (newline)
  (print "Copyright (C) 2014-2015 Nicholas M. Van Horn & Aaron Beveridge")
  (print "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.")
  (print "This is free software: you are free to change and redistribute it.")
  (print "There is NO WARRANTY, to the extent permitted by law.")
  (exit 0))

;; Routine responsible for setting up massmine's configuration
;; settings, etc.
(define (install-massmine params)
  (create-directory (alist-ref 'mm-cred-path params) #t))

;;; Helper function taken from my string "s" egg
(define (s-starts-with? prefix s #!optional (ignore-case #f))
  (if ignore-case
      (let ((mymatch (substring-index-ci prefix s)))
	(if (and mymatch (= mymatch 0)) #t #f))
      (let ((mymatch (substring-index prefix s)))
	(if (and mymatch (= mymatch 0)) #t #f))))

;; ##################################################################
;; Splash Screen
;; ##################################################################
;; Software info pretty-printed for the user
(define (splash-screen)
  (print "\n"
	 "        __  __               __  __ _                     \n"
	 "       |  \\/  | __ _ ___ ___|  \\/  (_)_ __   ___        \n"
	 "       | |\\/| |/ _` / __/ __| |\\/| | | \"_ \\ / _ \\    \n"
	 "       | |  | | (_| \\__ \\__ \\ |  | | | | | |  __/      \n"
	 "       |_|  |_|\\__,_|___/___/_|  |_|_|_| |_|\\___|       \n"
	 "                                                          \n"
	 "                Your Access To Big Data                   \n"
	 "\n\n"
	 "MassMine version " mm-version "\n"
	 "https://github.com/n3mo/massmine\n\n"
	 "Copyright (C) 2014-2015 Nicholas M. Van Horn & Aaron Beveridge\n"
	 "This program comes with ABSOLUTELY NO WARRANTY.\n"
	 "This is free software, and you are welcome to redistribute it\n"
	 "under certain conditions. Please see the included LICENSE file\n"
	 "for more information\n\n"))

;; This controls the main behavior of each run, dependent on the task
;; provided by the user at runtime (along with other command line
;; arguments) 
(define (task-dispatch curr-task)
  (cond
   ;; Authentication is a special case
   [(equal? curr-task "twitter-auth") (twitter-setup-auth P)]
   ;; Twitter tasks must be signed with oauth
   [(s-starts-with? "twitter" curr-task)
    (begin
      ;; Twitter related tasks require the same blanket authentication
      ;; prior to running
      (let* ((creds (twitter-auth P))
	     (twitter-app
	      (twitter-service
	       #:consumer-key (alist-ref 'consumer-key creds)
	       #:consumer-secret (alist-ref 'consumer-secret creds)))
	     (user-tokens
	      (twitter-token-credential
	       #:access-token (alist-ref 'access-token creds)
	       #:access-token-secret (alist-ref 'access-token-secret creds))))
	;; This intercepts all calls to Twitter and signs them with
	;; the user's oauth credentials
	(with-oauth
	 twitter-app user-tokens
	 (lambda ()
	   (cond
	    [(equal? curr-task "twitter-test") (twitter-test)]
	    [(equal? curr-task "twitter-stream")
	     (twitter-stream keywords locations language screen-name)]
	    [(equal? curr-task "twitter-sample") (twitter-sample)]
	    [(equal? curr-task "twitter-locations") (twitter-locations)]
	    [(equal? curr-task "twitter-trends") (twitter-trends locations)]
	    [(equal? curr-task "twitter-trends-nohash") (twitter-trends-nohash locations)]
	    [(equal? curr-task "twitter-timeline")
	     (twitter-timeline max-tweets screen-name)]
	    [(equal? curr-task "twitter-search")
	     (twitter-search max-tweets keywords locations language)]
	    [else (display "MassMine: Unknown task\n" (current-error-port))])))))]
   [else (display "MassMine: Unknown task\n" (current-error-port))])
  (exit 0))

;;; Just what you think. This gets things started
(define (main)

  ;; Install massmine's config file(s) if missing
  (install-massmine P)

  ;; Adjust for command line options. Update the master parameter alist
  (if (not max-tweets)
      (set! max-tweets (string->number (alist-ref 'count options))))
  (if (not keywords)
      (set! keywords (alist-ref 'pattern options)))
  (if (not global-max-seconds)
      (set! global-max-seconds (string->number (alist-ref 'time options))))
  (if (not locations)
      (set! locations (alist-ref 'geo options)))
  (if (not screen-name)
      (set! screen-name (alist-ref 'user options)))
  (if (not task)
      (set! task (alist-ref 'task options)))

  ;; Greet the user
  (if (and do-splash? output-to-file?) (splash-screen))

  ;; Get things done, printing to stdout or file contingent on how the
  ;; user called massmine
  (if output-to-file?
      (let ((out-file (alist-ref 'output options)))
  	(if (file-exists? out-file)
  	    ;; Abort if the output file already exists
  	    (begin (with-output-to-port (current-error-port)
  		     (lambda ()
  		       (print "Abort: Output file " out-file
  			      " already exists!")))
  		   (exit 1))
  	    ;; Else, get down to business
  	    (begin
  	      (with-output-to-file out-file
  		(lambda () (task-dispatch task))))))
      (task-dispatch task))
  (if output-to-file? (print "MassMine done!"))
  
  (exit 1))

(define options)
(define operands)
(define do-splash? #t)
(define output-to-file? #f)
(define max-tweets 999999999999999999)
(define global-max-seconds 999999999999999999)
(define task "twitter-stream")
(define keywords "")
(define locations "")
(define language "")
(define screen-name "")

;; Parse command line arguments
(set!-values (options operands)
	     (args:parse (command-line-arguments) opts))

;;(handle-exceptions exn (usage) (main))
(main)

;; End of file massmine
