#! /usr/bin/chicken-csi -s
;; ##################################################################
;;
;; MassMine: Your Access To Data
;; Copyright (C) 2014-2020  Nicholas M. Van Horn & Aaron Beveridge
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

;; Extensions. We need to import clucker here just so we can set! the
;; global variables used to sever the https connection with Twitter's
;; streaming API (which are defined in clucker)
(import (chicken file) (chicken process-context) (chicken condition)
	(chicken port) (chicken format) (chicken time))
(import openssl)
(import args clucker oauth-client
	srfi-19 pathname-expand utf8)

;; Current version of software
(define mm-version "1.3.0 (2020-05-15)")

;; For future command line arguments and options
(define options)
(define operands)

;; MassMine parameters
(define do-splash?		(make-parameter #t))
(define output-to-file?		(make-parameter #f))
(define task			(make-parameter ""))
(define keywords		(make-parameter ""))
(define locations		(make-parameter ""))
(define language		(make-parameter ""))
(define user-info		(make-parameter ""))
(define install-path		(make-parameter (pathname-expand "~/.config/massmine")))
(define custom-cred-path	(make-parameter #f))
(define config-file             (make-parameter #f))
(define output-file             (make-parameter #f))
(define date                    (make-parameter "2100-01-01"))
(define testing?                (make-parameter #f))
(define server                  (make-parameter #f))

(include "./modules/twitter")
(import massmine-twitter)
(include "./modules/wikipedia")
(import massmine-wikipedia)
;; (include "./modules/google")
;; (import massmine-google)
(include "./modules/tumblr")
(import massmine-tumblr)
(include "./modules/web")
(import massmine-web)
(include "./modules/server")

;; Useful examples, displayed when 'massmine -h examples' is ran
(define massmine-examples #<<END
The following examples demonstrate how to compose options into
customized data requests. For more information on any massmine
option, run

    massmine -h <option>

For example, to see what tasks are available run

    massmine -h task

EXAMPLES
--------

Collect 200 tweets from Twitter in real time matching the keyword
'love'. Write the results to the file my_data.json

    massmine -t twitter-stream -c 200 --query love -o my_data.json

Search for up to 100 previously existing tweets matching 'potato'
or 'climbing'. Limit results to english tweets. Print results to
standard output (not to file). Notice the single quotes around
complex queries

    massmine -t twitter-search -c 100 -q 'potato OR climbing' -l en

Retrieve the current top-10 trends in the world
    
    massmine -t twitter-trends -g 1

Same as above, but hide the MassMine splash screen

    massmine -t twitter-trends -g 1 --no-splash

Retrieve the current top-50 trends, excluding #hashtags, in
New York, New York. Write results to the file NY_trends.json

    massmine -t twitter-trends-nohash -g 2459115 -o NY_trends.json

END
)

;;; The following list contains all defined command line options
;;; available to the user. For example, (h help) makes all of the
;;; following equivalent options available at runtime: -h, -help, --h,
;;; --help. These are used by the "args" egg.
(define opts
  (list (args:make-option (h help)    #:none "Help information"
			  (massmine-help options))
	(args:make-option (v version)  #:none "Version information"
			  (print-version))
	(args:make-option (p project)  #:required  "Create project"
			  (create-project-directory))
	(args:make-option (a auth)  #:required "Credentials file"
			  (custom-cred-path #t))	
	(args:make-option (o output)  #:required  "Write to file"
			  (output-to-file? #t))
	(args:make-option (t task)  #:required "Task name"
			  (task #f))
	(args:make-option (q query)  #:required "Query string"
			  (keywords #f))
	(args:make-option (c count)  #:required "Number of records"
			  (max-tweets #f))
	(args:make-option (d dur)  #:required "Max runtime"
			  (global-max-seconds #f))
	(args:make-option (g geo)  #:required "Location"
			  (locations #f))
	(args:make-option (l lang)  #:required "Language"
			  (language #f))
	(args:make-option (u user)  #:required "Screen name"
			  (user-info #f))
	(args:make-option (date)  #:required "Date (or date range)"
			  (date arg))
	(args:make-option (config)  #:required "Config file"
			  (config-file #t))
	(args:make-option (no-splash)  #:none "Inhibit splash screen"
			  (do-splash? #f))
	(args:make-option (server)  #:required "Start in server mode"
			  (server #f))
	(args:make-option (test)  #:required "Development tests"
			  (testing? #t))))

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
     (print "    'massmine -h task' will, for example, display help about the 'task' option")
     (print "or  'massmine -h task-options' for options supported by each task ")
     (print "or  'massmine -h examples' for detailed examples")
     (newline)
     (print "Full documentation can be found at http://www.massmine.org")
     (print "Report bugs at https://github.com/n3mo/massmine")))
 (exit 1))

;; Prints the current version of massmine, and useful info
(define (print-version)
  (print "massmine " mm-version)
  (print "http://www.massmine.org")
  (print "https://github.com/n3mo/massmine")
  (newline)
  (print "Copyright (C) 2014-2020 Nicholas M. Van Horn & Aaron Beveridge")
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
	(print "Example(s): 'massmine --task=twitter-locations'")
	(print "            'massmine -t twitter-locations'")
	(newline)
	(print "Available tasks:")
	(print "----------------")
	;; (for-each (lambda (task)
	;; 	    (display (sprintf "~A~A~A -- ~A"
	;; 			      "\033[94m"
	;; 			      (car task) "\033[0m" (cdr task)))
	;; 	    (newline))
	;; 	  google-task-descriptions)
	;; (newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[92m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  tumblr-task-descriptions)
	(newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[94m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  twitter-task-descriptions)
	(newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[92m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  web-task-descriptions)
	(newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[94m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  wikipedia-task-descriptions)
	(newline))]

     [(equal? topic "task-options")
      (begin
	(print "Each task supports zero or more options. Options")
	(print "marked with a * are required. For options marked")
	(print "with a + choose only one")
	(newline)
	;; (for-each (lambda (task)
	;; 	    (display (sprintf "~A~A~A -- ~A"
	;; 			      "\033[94m"
	;; 			      (car task) "\033[0m" (cdr task)))
	;; 	    (newline))
	;; 	  google-task-options)
	;; (newline)

	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[92m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  tumblr-task-options)
	(newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[94m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  twitter-task-options)
	(newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[92m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  web-task-options)
	(newline)
	(for-each (lambda (task)
		    (display (sprintf "~A~A~A -- ~A"
				      "\033[94m"
				      (car task) "\033[0m" (cdr task)))
		    (newline))
		  wikipedia-task-options)
	(newline))]
     [(equal? topic "output")
      (begin
	(print "If included, output is written to file (otherwise, standard out)")
	(print "\nExample(s): 'massmine --output=twitterdata.json'")
	(print "            'massmine -o twitterdata.json'")
	(newline))]
     [(equal? topic "query")
      (begin
	(print "Keyword(s) strings to be applied to data searches. Keyword format")
	(print "and operators depends on the data source. See www.massmine.org")
	(print "for task-specific information")
	(print "\nExample(s): 'massmine --query=buckeyes'")
	(print "            'massmine -q buckeyes'")
	(newline))]
     [(equal? topic "count")
      (begin
	(print "Request a number of records for tasks that accept limits")
	(print "\nExample(s): 'massmine --count=100'")
	(print "            'massmine -c 100'")
	(newline))]
     [(equal? topic "dur")
      (begin
	(print "Stop collection of records after DUR has elapsed.")
	(print "The format of DUR depends on the task. See www.massmine.org")
	(print "for task-specific information")
	(print "\nExample(s): 'massmine --dur='2016-10-11 14:30:00''")
	(print "            'massmine -d '2016-10-11 14:30:00''")
	(newline))]
     [(equal? topic "geo")
      (begin
	(print "Limit collection to geographic regions for tasks that support it.")
	(print "See www.massmine.org for task-specific information")
	(print "\nExample(s): 'massmine --geo=-74,40,-73,41'")
	(print "            'massmine -g -74,40,-73,41'")
	(newline))]
     [(equal? topic "lang")
      (begin
	(print "Limit collection to a given language for tasks that support it.")
	(print "See www.massmine.org for task-specific information")
	(print "\nExample(s): 'massmine --lang=en'")
	(print "            'massmine -l en'")
	(newline))]
     [(equal? topic "user")
      (begin
	(print "Limit collection to specific users for tasks that support it.")
	(print "See www.massmine.org for task-specific information")
	(print "\nExample(s): 'massmine --user=ladygaga'")
	(print "            'massmine -u ladygaga'")
	(newline))]
     [(equal? topic "date")
      (begin
	(print "Limit collection to a specific date(s) for tasks that support it.")
	(print "See www.massmine.org for task-specific information")
	(print "\nExample(s): 'massmine --date=2015-10-11'"))]
     [(equal? topic "config")
      (begin
	(print "Control massmine's behavior with a configuration file instead")
	(print "of using command line options.")
	(print "See http://www.massmine.org/docs/config.html#alternative-configuration-file")
	(print "for more information")
	(print "\nExample(s): 'massmine --config=my_config.txt'"))]
     [(equal? topic "no-splash")
      (begin
	(print "Suppress massmine's startup banner")
	(print "\nExample(s): 'massmine --no-splash'"))]
     [(equal? topic "examples")
      (begin
	(print massmine-examples))]
     [(equal? topic "version")
      (begin
	(print "Displays version information for the current MassMine installation")
	(print "\nExample(s): 'massmine --version'")
	(print "             'massmine -v'"))]
     [(equal? topic "project")
      (begin
	(print "Creates a directory tree for managing a data")
	(print "data collection and analysis project. Creates a")
	(print "directory named by the supplied argument. The created")
	(print "directory itself contains three sub-directories:")
	(print "(1) 'data', (2) 'log', and (3) 'research' for managing")
	(print "your project's data files, records/logs, and")
	(print "research/analysis information, respectively.")
	(print "\nExample(s): 'massmine --project=election'")
	(print "             'massmine -p election'")
	(print "\n(both commands will create a directory tree")
	(print "called 'election' as described above)"))]
     [(equal? topic "auth")
      (begin
	(print "Manually specifies an authorization file. MassMine automatically")
	(print "manages login credentials for the user. If, for example, you")
	(print "would like to use different credentials for different data")
	(print "collection tasks, or would like to simply save the")
	(print "information to a custom location, specify the path/file with")
	(print "this option. This option can be used in conjunction with data")
	(print "collection AND with authentification tasks (e.g., twitter-auth).")
	(print "\nExample(s): 'massmine --auth=my_credentials'")
	(print "            'massmine -a my_credentials'")
	(print "\nA common workflow is to create an auth file for a new project")
	(print "and then immediately use it for data collection:")
	(print "            'massmine -a my_auth -t twitter-auth'")
	(print "            'massmine -a my_auth -t twitter-locations'"))]
     [else (usage)])
    (exit 0)))

;; Tests. These can be run with the --test flag by users to ensure
;; that their installation is working properly, or by developers
(define (run-tests file)
  (handle-exceptions exn
      (begin
	(display "This option is for development purposes.\n"
		 (current-error-port))
	(display "You must provide a valid test file\n"
		 (current-error-port))
	(display "Example: massmine.scm --test ./tests/run.scm\n"
		 (current-error-port))
	(exit 1))
    (load file)))

;; Routine responsible for setting up massmine's configuration
;; settings, etc.
(define (install-massmine)
  (create-directory (install-path) #t))

;; Creates a convenient, albeit not-required, directory structure for
;; creating and managing massmine projects
(define (create-project-directory)
  (let ((project
	 (let loop ((myargs (command-line-arguments)))
	   (cond
	    [(null? myargs) ""]
	    [(equal? (car myargs) "-p") (car (cdr myargs))]
	    [(s-starts-with? "--project=" (car myargs) "--project=")
	     (cadr (string-split (car myargs) "="))]
	    [else (loop (cdr myargs))]))))
    (create-directory project)
    (create-directory (string-append project "/" "research"))
    (create-directory (string-append project "/" "data"))
    (create-directory (string-append project "/" "log"))
    (exit 0)))


;; Helper function taken from my string "s" egg
(define (s-starts-with? prefix s #!optional (ignore-case #f))
  (if ignore-case
      (let ((mymatch (substring-index-ci prefix s)))
	(if (and mymatch (= mymatch 0)) #t #f))
      (let ((mymatch (substring-index prefix s)))
	(if (and mymatch (= mymatch 0)) #t #f))))

;; Returns the number of seconds in local time until a given date is
;; reached. Date/time should be supplied as "YYYY-MM-DD HH:MM:SS"

;; There is currently a bug in Chicken (caused by differences in the
;; behavior of strptime on Linux vs OS X... see my ticket here:
;; https://bugs.call-cc.org/ticket/1217 . Until this is fixed, I
;; have different code for each OS

;; OS X version. This version is hopefully safe to remove now that
;; we're using the srfi-19 date procedures
;; (define (update-max-seconds! timestr)
;;   (handle-exceptions exn
;; 	(abort "Invalid duration format. 'YYYY-MM-DD HH:MM:SS TIMEZONE' expected") 
;;     (let ((deadline
;; 	     (- (local-time->seconds (string->time timestr "%Y-%m-%d %H:%M:%S %Z"))
;; 		(current-seconds))))
;; 	deadline)))

;; srfi-19 version
(define (date-string->seconds timestr)
  (handle-exceptions exn
      (begin
	(display "\nInvalid duration format. 'YYYY-MM-DD HH:MM:SS'expected\n"
		 (current-error-port))
	(exit 1)) 
    (time->seconds (date-difference
		    (string->date timestr "~Y-~m-~d ~H:~M:~S")
		    (seconds->date (current-seconds))))))


;; Helper function for configure-from-file. Expects a list with
;; (option value), as described in opts. Blank lines in the config
;; file show up as empty lists (and we skip these)
(define (update-option! opt-value)
  (unless (null? opt-value)
    (let ((opt (car opt-value))
	  (optval (cadr opt-value)))
      (if (equal? opt "auth")
	  (custom-cred-path (pathname-expand optval)))
      (if (equal? opt "output")
	  (begin
	    (output-to-file? #t)
	    (output-file optval)))
      (if (equal? opt "task")
	  (task optval))
      (if (equal? opt "query")
	  (keywords optval))
      (if (equal? opt "count")
	  (max-tweets (string->number optval)))
      (if (equal? opt "dur")
	  (global-max-seconds (date-string->seconds optval)))
      (if (equal? opt "geo")
	  (locations optval))
      (if (equal? opt "lang")
	  (language optval))
      (if (equal? opt "user")
	  (user-info optval))
      (if (equal? opt "date")
	  (date optval))
      (if (equal? opt "no-splash")
	  (do-splash #f)))))

;; Parse configuration file. User can specify a config file instead of
;; supplying command line options. The config file must be formatted
;; as one line per option, with each line following the format
;; "option = value", where option must be a full command line options
;; (e.g., task, not just t) and value is whatever you'd like to set
;; the option as. This procedure is ran for its side effects!
(define (configure-from-file)
  (let* ((config-lines
	  (with-input-from-file (config-file) (lambda () (read-lines))))
	 (params
	  (map (lambda (x)
		 (map string-trim-both (string-split x "="))) config-lines)))
    (for-each update-option! params)))

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
	 "                    Your Access To Data                   \n"
	 "\n\n"
	 "MassMine version " mm-version "\n"
	 "https://github.com/n3mo/massmine\n\n"
	 "Copyright (C) 2014-2019 Nicholas M. Van Horn & Aaron Beveridge\n"
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
   [(equal? curr-task "twitter-auth")
    (twitter-setup-auth (custom-cred-path))]
   [(equal? curr-task "tumblr-auth")
    (tumblr-setup-auth (custom-cred-path))]
   ;; Twitter tasks must be signed with oauth
   [(s-starts-with? "twitter" curr-task)
    (begin
      ;; Twitter related tasks require the same blanket authentication
      ;; prior to running
      (let* ((creds (twitter-auth (custom-cred-path)))
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
	   ;; Fancy pants way of running commands as defined in the
	   ;; twitter module. So fancy that it only works in
	   ;; interpreted code... If I figure out how to make this
	   ;; work in compiled code I'll switch back
	   ;; (if (alist-ref (string->symbol curr-task) twitter-tasks)
	   ;;     ;; This does the heavy lifting, calling the right procedure
	   ;;     (eval (alist-ref (string->symbol curr-task) twitter-tasks))
	   ;;     ;; (eval (alist-ref (string->symbol curr-task) twitter-tasks)
	   ;;     ;; 	     (module-environment 'massmine-twitter))
	   ;;     (display "MassMine: Unknown task\n" (current-error-port)))
	   (cond
	    [(equal? curr-task "twitter-stream")
	     (twitter-stream (keywords) (locations) (language) (user-info))]
	    [(equal? curr-task "twitter-sample") (twitter-sample)]
	    [(equal? curr-task "twitter-locations") (twitter-locations)]
	    [(equal? curr-task "twitter-trends") (twitter-trends (locations))]
	    [(equal? curr-task "twitter-trends-nohash") (twitter-trends-nohash (locations))]
	    [(equal? curr-task "twitter-user")
	     (twitter-timeline (max-tweets) (user-info))]
	    [(equal? curr-task "twitter-friends")
	     (twitter-friends-list (user-info))]
	    [(equal? curr-task "twitter-followers")
	     (twitter-followers-list (user-info))]
	    [(equal? curr-task "twitter-search")
	     (twitter-search (max-tweets) (keywords) (locations) (language))]
	    [(equal? curr-task "twitter-rehydrate")
	     (twitter-rehydrate (keywords))]
	    ;; [else (display "MassMine: Unknown task\n" (current-error-port))])))))]
	    [else (abort (make-property-condition 'exn 'message "Unknown task requested"))])))))]

   ;; Tumblr tasks must be signed with oauth
   [(s-starts-with? "tumblr" curr-task)
    (begin
      ;; Tumblr related tasks require the same blanket authentication
      ;; prior to running (not entirely true. Some require oauth, some
      ;; do not. But signing everything with oauth works fine).
      (let* ((creds (tumblr-auth (custom-cred-path)))
	     (tumblr-app
	      (tumblr-service
	       #:consumer-key (alist-ref 'consumer-key creds)
	       #:consumer-secret (alist-ref 'consumer-secret creds)))
	     (user-tokens
	      (tumblr-token-credential
	       #:access-token (alist-ref 'access-token creds)
	       #:access-token-secret (alist-ref 'access-token-secret creds))))
	;; This intercepts all calls to Tumblr and signs them with
	;; the user's oauth credentials
	(with-oauth
	 tumblr-app user-tokens
	 (lambda ()
	   (cond
	    [(equal? curr-task "tumblr-blog-info")
	     (tumblr-blog-info (keywords) (alist-ref 'consumer-key creds))]
	    [(equal? curr-task "tumblr-posts")
	     (tumblr-posts (keywords) (max-tweets) (alist-ref 'consumer-key creds))]
	    [(equal? curr-task "tumblr-tag")
	     (tumblr-tag (keywords) (max-tweets) (alist-ref 'consumer-key creds))]
	    ;; [else (display "MassMine: Unknown task\n" (current-error-port))])))))]
	    [else (abort (make-property-condition 'exn 'message "Unknown task requested"))])))))]

   [(s-starts-with? "web" curr-task)
    (cond
     [(equal? curr-task "web-text") (web-text (keywords))])]

   [(s-starts-with? "wikipedia" curr-task)
    (cond
     [(equal? curr-task "wikipedia-search") (wikipedia-search (keywords) (language))]
     [(equal? curr-task "wikipedia-text") (wikipedia-text (keywords) (language))]
     [(equal? curr-task "wikipedia-views") (wikipedia-views (keywords) (date))]
     [(equal? curr-task "wikipedia-page-links") (wikipedia-page-links (keywords) (language))]
     [(equal? curr-task "wikipedia-trends") (wikipedia-trends (date))]
     [else (abort (make-property-condition 'exn 'message "Unknown task requested"))])]
   ;; [(s-starts-with? "google" curr-task)
   ;;  (cond
   ;;   ;; [(equal? curr-task "google-trends") (google-trends (date))]
   ;;   [(equal? curr-task "google-country-trends") (google-country-trends)]
   ;;   [else (abort (make-property-condition 'exn 'message "Unknown task requested"))])]
   ;; [else (display "MassMine: Unknown task\n" (current-error-port)) (usage)]))
   [else (abort (make-property-condition 'exn 'message "Unknown task requested"))]))

;;; Just what you think. This gets things started
(define (main)

  ;; If testing? is #t, then we run our tests and quit
  (when (testing?)
    (let ((test-file (alist-ref 'test options)))
      (if (file-exists? test-file)
	  (run-tests test-file)
	  (begin
	    (display "This option is for development purposes.\n"
		     (current-error-port))
	    (display "You must provide a valid test file\n"
		     (current-error-port))
	    (display "Example: massmine.scm --test ./tests/run.scm\n"
		     (current-error-port))
	    (exit 1)))))

  ;; Install massmine's config file(s) if missing
  (install-massmine)

  ;; If the user has supplied configuration file path, we load it
  ;; first. This way, the config file's behavior is trumped by any
  ;; command line arguments
  (if (config-file)
      (begin
	(config-file (alist-ref 'config options))
	(handle-exceptions exn
	    (begin
	      (display "An error occurred while processing file " (current-error-port))
	      (display (config-file) (current-error-port))
	      (display "\nCheck for malformed entries\n" (current-error-port))
	      (exit 1))
	    (configure-from-file))))

  ;; Adjust for command line options. Update the master parameter alist
  (if (not (max-tweets))
      (max-tweets (string->number (alist-ref 'count options))))
  (if (not (keywords))
      (keywords (alist-ref 'query options)))
  (if (not (global-max-seconds))
      (global-max-seconds (date-string->seconds (alist-ref 'dur options))))
  (if (not (locations))
      (locations (alist-ref 'geo options)))
  (if (not (language))
      (language (alist-ref 'lang options)))
  (if (not (user-info))
      (user-info (alist-ref 'user options)))
  (if (not (task))
      (task (alist-ref 'task options)))
  (if (not (server))
      (server (alist-ref 'server options)))
  (if (custom-cred-path)
      (custom-cred-path (alist-ref 'auth options)))

  ;; Run in server mode? If so, intercept control now. Nothing below
  ;; this will run
  (when (server)
    (massmine-server #:port (string->number (server)))
    (exit 0))

  ;; Greet the user
  (if (and (do-splash?) (output-to-file?)) (splash-screen))

  ;; Get things done, printing to stdout or file contingent on how the
  ;; user called massmine
  (if (output-to-file?)
      (let ((out-file (if (output-file) (output-file) (alist-ref 'output options))))
  	(if (file-exists? out-file)
  	    ;; Abort if the output file already exists
  	    (begin (with-output-to-port (current-error-port)
  		     (lambda ()
  		       (print "Abort: Output file " out-file
  			      " already exists!")))
  		   (exit 1))
  	    ;; Else, get down to business
  	    (handle-exceptions exn
		(begin
		  (display "MassMine: The following error occurred: " (current-error-port))
		  (display ((condition-property-accessor 'exn 'message) exn)
			   (current-error-port))
		  (display "\n" (current-error-port)))
	      (with-output-to-file out-file
		(lambda () (task-dispatch (task))
			(exit 0))))))
      ;; This call does the heavy lifting
      (handle-exceptions exn
	  (begin
	    (display "MassMine: The following error occurred: " (current-error-port))
	    (display ((condition-property-accessor 'exn 'message) exn)
		     (current-error-port))
	    (display "\n" (current-error-port)))
	(task-dispatch (task))
	(exit 0)))
  (if (output-to-file?) (print "MassMine done!"))
  
  (exit 1))

;; Parse command line arguments
(set!-values (options operands)
	     (args:parse (command-line-arguments) opts))

;;(handle-exceptions exn (usage) (main))
(main)

;; End of file massmine.scm
