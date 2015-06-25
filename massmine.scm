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

;; Extensions. We need to import clucker here just so we can set! the
;; global variables used to sever the https connection with Twitter's
;; streaming API (which are defined in clucker)
(require-extension args clucker openssl posix oauth-client openssl extras)

;; Current version of software
(define mm-version "0.9.0 (2015-06-24)")

;; For future command line arguments and options
(define options)
(define operands)

;; MassMine parameters
(define do-splash?	(make-parameter #t))
(define output-to-file? (make-parameter #f))
(define task		(make-parameter ""))
(define keywords	(make-parameter ""))
(define locations	(make-parameter ""))
(define language	(make-parameter ""))
(define user-info	(make-parameter ""))

(include "./modules/twitter")
(import massmine-twitter)

;; Master parameter alist
(define P '((mm-cred-path . "~/.config/massmine")))

;;; The following list contains all defined command line options
;;; available to the user. For example, (h help) makes all of the
;;; following equivalent options available at runtime: -h, -help, --h,
;;; --help. These are used by the "args" egg.
(define opts
  (list (args:make-option (h help)    #:none "Help information"
			  (massmine-help options))
	(args:make-option (v version)  #:none "Version information"
			  (print-version))
	(args:make-option (o output)  (required: "FILE")  "Write to file"
			  (output-to-file? #t))
	(args:make-option (t task)  (required: "TASK") "Task name"
			  (task #f))
	(args:make-option (q query)  (required: "QUERY") "Query string"
			  (keywords #f))
	(args:make-option (c count)  (required: "NUM") "Number of records"
			  (max-tweets #f))
	(args:make-option (d dur)  (required: "SECOND") "Max runtime"
			  (global-max-seconds #f))
	(args:make-option (g geo)  (required: "LOCATION") "Location"
			  (locations #f))
	(args:make-option (l lang)  (required: "LANG") "Language"
			  (language #f))
	(args:make-option (u user)  (required: "NAME") "Screen name"
			  (user-info #f))
	(args:make-option (no-splash)  #:none "Inhibit splash screen"
			  (do-splash? #f))))

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
     (print "See 'massmine -h <option>' to read about a specific topic")
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

;; Detailed help for UI
(define (massmine-help topic)
  (let ((topic
	 (let loop ((myargs (command-line-arguments)))
	   (cond
	    [(null? myargs) ""]
	    [(and (not (null? (cdr myargs)))
	      (or (equal? (car myargs) "-h")
		  (equal? (car myargs) "--help"))) (car (cdr myargs))] 
	    [else (loop (cdr myargs))]))))
    (cond
     [(equal? topic "task")
      (begin
	(print "Control massmine's behavior by setting a task:")
	(print "Example: massmine -t twitter-sample")
	(newline)
	(print "Available tasks:")
	(print "----------------")
	(for-each (lambda (task)
		    (display (sprintf "~A \t\t-- ~A" (car task) (cdr task)))
		    (newline))
		  twitter-task-descriptions)
	(exit 0))]
     [else (usage)])))

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
	   (if (alist-ref (string->symbol curr-task) twitter-tasks)
	       ;; This does the heavy lifting, calling the right procedure
	       (eval (alist-ref (string->symbol curr-task) twitter-tasks))
	       (display "MassMine: Unknown task\n" (current-error-port)))))))]
   [else (display "MassMine: Unknown task\n" (current-error-port)) (usage)])
  (exit 0))

;;; Just what you think. This gets things started
(define (main)

  ;; Install massmine's config file(s) if missing
  (install-massmine P)

  ;; Adjust for command line options. Update the master parameter alist
  (if (not (max-tweets))
      (max-tweets (string->number (alist-ref 'count options))))
  (if (not (keywords))
      (keywords (alist-ref 'query options)))
  (if (not (global-max-seconds))
      (global-max-seconds (string->number (alist-ref 'dur options))))
  (if (not (locations))
      (locations (alist-ref 'geo options)))
  (if (not (language))
      (language (alist-ref 'lang options)))
  (if (not (user-info))
      (user-info (alist-ref 'user options)))
  (if (not (task))
      (task (alist-ref 'task options)))

  ;; Greet the user
  (if (and (do-splash?) (output-to-file?)) (splash-screen))

  ;; Get things done, printing to stdout or file contingent on how the
  ;; user called massmine
  (if (output-to-file?)
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
  		(lambda () (task-dispatch (task)))))))
      (task-dispatch (task)))
  (if (output-to-file?) (print "MassMine done!"))
  
  (exit 1))

;; Parse command line arguments
(set!-values (options operands)
	     (args:parse (command-line-arguments) opts))

;;(handle-exceptions exn (usage) (main))
(main)

;; End of file massmine.scm
