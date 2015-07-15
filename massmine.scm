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
(define mm-version "0.9.2 (2015-07-15)")

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
(define install-path		(make-parameter "~/.config/massmine"))
(define custom-cred-path	(make-parameter #f))
(define config-file             (make-parameter #f))
(define output-file             (make-parameter #f))

(include "./modules/twitter")
(import massmine-twitter)

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

Same as above, but collect the maximum number of tweets for 10 seconds

    massmine -t twitter-stream -d 10 -q love -o my_data.json

Search for up to 100 previously existing tweets matching 'potato'
or 'climbing'. Limit results to english tweets. Print results to
standard output (not to file). Notice the single quotes around
complex queries

    massmine -t twitter-search -c 100 -q 'potato OR climbing' -l en

Retrieve the current top-10 trends in the world
    
    massmine -t twitter-trends -g 1

Same as above, but hide the MassMine splash screen

    massmine -t twitter-trends -g 1 --no-splash

Retrieve the current top-10 trends, excluding #hashtags, in
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
	(args:make-option (p project)  (required: "NAME")  "Create project"
			  (create-project-directory))
	(args:make-option (a auth)  (required: "FILE") "Credentials file"
			  (custom-cred-path #t))	
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
	(args:make-option (config)  (required: "FILE") "Config file"
			  (config-file #t))
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
     (print "or  'massmine -h examples' for detailed examples")
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
	(print "Example(s): 'massmine --task=twitter-locations'")
	(print "            'massmine -t twitter-locations'")
	(newline)
	(print "Available tasks:")
	(print "----------------")
	(for-each (lambda (task)
		    (display (sprintf "~A \t\t-- ~A" (car task) (cdr task)))
		    (newline))
		  twitter-task-descriptions))]
     [(equal? topic "output")
      (begin
	(print "If included, output is written to file (otherwise, standard out)")
	(print "Example(s): 'massmine --output=twitterdata.json'")
	(print "            'massmine -o twitterdata.json'")
	(newline))]
     [(equal? topic "query")
      (begin
	(print "Keyword(s) strings to be applied to data searches")
	(print "Example(s): 'massmine --query=buckeyes'")
	(print "            'massmine -q buckeyes'")
	(newline))]
     [(equal? topic "count")
      (begin
	(print "Request a number of records for tasks that accept limits")
	(print "Example(s): 'massmine --count=100'")
	(print "            'massmine -c 100'")
	(newline))]
     [(equal? topic "dur")
      (begin
	(print "Stop collection of records after DUR seconds")
	(print "Example(s): 'massmine --dur=3600'")
	(print "            'massmine -d 3600'")
	(newline))]
     [(equal? topic "geo")
      (begin
	(print "Limit collection to geographic regions for tasks that allow it")
	(print "Example(s): 'massmine --geo=-74,40,-73,41'")
	(print "            'massmine -g -74,40,-73,41'")
	(newline))]
     [(equal? topic "lang")
      (begin
	(print "Limit collection to a given language for tasks that allow it")
	(print "Example(s): 'massmine --lang=en'")
	(print "            'massmine -l en'")
	(newline))]
     [(equal? topic "user")
      (begin
	(print "Limit collection to specific users for tasks that allow it")
	(print "Example(s): 'massmine --user=ladygaga'")
	(print "            'massmine -u ladygaga'")
	(newline))]
     [(equal? topic "examples")
      (begin
	(print massmine-examples))]
     [else (usage)])
    (exit 0)))

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

;; Helper function for configure-from-file. Expects a list with
;; (option value), as described in opts. Blank lines in the config
;; file show up as empty lists (and we skip these)
(define (update-option! opt-value)
  (unless (null? opt-value)
    (let ((opt (car opt-value))
	  (optval (cadr opt-value)))
      (if (equal? opt "auth")
	  (custom-cred-path optval))
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
	  (global-max-seconds (string->number optval)))
      (if (equal? opt "geo")
	  (locations optval))
      (if (equal? opt "lang")
	  (language optval))
      (if (equal? opt "user")
	  (user-info optval))
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
   [(equal? curr-task "twitter-auth")
    (twitter-setup-auth (custom-cred-path))]
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
	    [(equal? curr-task "twitter-search")
	     (twitter-search (max-tweets) (keywords) (locations) (language))]
	    [else (display "MassMine: Unknown task\n" (current-error-port))])))))]
   [else (display "MassMine: Unknown task\n" (current-error-port)) (usage)])
  (exit 0))

;;; Just what you think. This gets things started
(define (main)

  ;; Install massmine's config file(s) if missing
  (install-massmine)

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
  (if (custom-cred-path)
      (custom-cred-path (alist-ref 'auth options)))
  ;; If the user has supplied configuration file path, we load it
  ;; last. This way, the config file's behavior trumps all other
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
		  (display "MassMine: The following error occurred:\n" (current-error-port))
		  (display ((condition-property-accessor 'exn 'message) exn)
			   (current-error-port))
		  (display "\n" (current-error-port)))
	      (with-output-to-file out-file
		(lambda () (task-dispatch (task)))))))
      ;; This call does the heavy lifting
      (handle-exceptions exn
	  (begin
	    (display "MassMine: The following error occurred:\n" (current-error-port))
	    (display ((condition-property-accessor 'exn 'message) exn)
		     (current-error-port))
	    (display "\n" (current-error-port)))
	  (task-dispatch (task))))
  (if (output-to-file?) (print "MassMine done!"))
  
  (exit 1))

;; Parse command line arguments
(set!-values (options operands)
	     (args:parse (command-line-arguments) opts))

;;(handle-exceptions exn (usage) (main))
(main)

;; End of file massmine.scm
