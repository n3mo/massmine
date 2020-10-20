# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased] - [unreleased]

### Added
- Users with paid access to Twitter's premium APIs can now perform full historical searches using the "fullarchive" premium endpoint. This is made available through the new twitter-search-fullarchive task. Users wishing to use this new task will need to re-run the twitter-auth task in massmine after purchasing access and setting up an account at Twitter.
- You can now dehydrate an existing massmine Twitter dataset with the `twitter-dehydrate` task. This task takes a massmine JSON file using the --input option, and returns a list of tweet IDs, one per line. These IDs can be shared with fellow researchers in a limited capacity as a way to share datasets. These IDs can be "rehydrated" back to full tweet objects with the `twitter-rehydrate` task.

## [1.2.2] - 2020-10-19

### Changed
- Fixes bug that prevented massmine from parsing config files used through the --config option.

## [1.2.1] - 2020-06-01

### Changed
- Twitter authentication setup bug fix
- Tumblr authentication setup bug fix

## [1.2.0] - 2020-05-15

### Added
- You can now "rehydrate" tweets using Twitter's statuses-lookup API endopoint using the `twitter-rehydrate` task. This is the preferred method for retrieving older tweets, and Twitter's only allowed method for sharing tweets with other researchers. Now you can share tweet IDs (up to Twitter's allowed limits, and subject to their terms and conditions), and your recipient can "rehydrate" the tweets from the IDs that you have shared.
- Server mode added. MassMine can now to be remotely controlled over tcp sockets. See documentation at https://www.massmine.org/docs/server.html
- Explicit utf8 support added. This likely has little effect on massmine's behavior to date, as it engaged in little-to-no processing of text data. This is more of a forward-facing addition to ensure future work behaves appropriately when working with utf8 text.

### Changed
- The entire codebase has been upgraded to work with Chicken Scheme v5.x. This mostly reflects changes in how Chicken 5.x handles modules. While important for the project, this change should have no effect on the user.
- All google tasks disabled until suitable access points are discovered.
- Pre-compiled massmine binaries are now statically linked thanks to bump to Chicken version 5.

## [1.1.0] - 2018-06-06

### Added
- Wikipedia's 1000 most viewed articles for any given month or day now accessible via the `wikipedia-trends` task!
- With bug fix for wikipedia-views (see below), there is now better support for daily page view statistics from wikipedia. This includes day-by-day and even hour-by-hour support thanks to wikimedia's API. This changes the call syntax for wikipedia-views (changes are reflected in `massmine -h task-options`)
- Unit tests! MassMine can now run tests for each module. Tests are included in the tests directory. Example: ./massmine.scm --test ./tests/run.scm

### Changed
- `google-trends` task removed until a new suitable access point is discovered.

### Fixed
- wikipedia-views task updated to fix broken functionality. Now fetches data from the wikimedia API. 

## [1.0.2] - 2016-09-07

### Fixed
- Bug fix for twitter-user task. Rate limits were not properly handled previously (they were ignored due to the nature of the bug).

### Changed
- Doubled the ssl shutdown timeout. Previously, occasional connections were taking too long to shutdown, leading to problems for data collections that queried an API many times over a long period of time. By increasing MassMine's tolerance of such connections from 2 to 4 minutes, we are more robust to the occasional timeout hiccup.

## [1.0.1] - 2016-07-14

### Added
- Version info now prints http://www.massmine.org

### Fixed
- Removed debug print statement that was causing the wikipedia-text task to output a boolean #t before writing the JSON result.

## [1.0.0] - 2016-07-08

### Added
- Additional built-in help for undocumented command line options
- References to online help in built-in help
- Wikipedia-text now supports multiple languages. Use --lang to specify the Wikipedia language URL (e.g., --lang=es for the Spanish Wikipedia URL https://es.wikipedia.org
- Wikipedia-views now supports multiple languages. Use --lang to specify the Wikipedia language URL (e.g., --lang=es for the Spanish Wikipedia URL https://es.wikipedia.org

### Changed
- twitter-search no longer writes arrays of tweets. For consistency with other tasks, it now writes one tweet per line
- twitter-user no longer writes arrays of tweets. For consistency with other tasks, it now writes one tweet per line
- twitter-trends no longer writes arrays of tweets. For consistency with other tasks, it now writes one tweet per line
- twitter-trends-nohash no longer writes arrays of tweets. For consistency with other tasks, it now writes one tweet per line
- Updated documentation to reflect that Twitter now returns top 50 trends instead of top 10
- Simplified command line option required arguments in help screen (i.e., made the required text generic, as it should be, given how different tasks expect differently-formatted options

## [0.11.0] - 2016-03-11

### Added
- Web (url) scraping!
- web-text: Retrieves one or more urls and returns the text stripped of all html markup, the url itself, and a timestamp.
- Timestamp added to each output entry from twitter-trends and twitter-trends-nohash
- Added missing(!) support for the --date CL option to config file mode

### Changed
- MassMine tagline changed
- Copyright year updated
- Default --date param set to valid date far into the future
- Implicit home directory expansion in file paths now explicit via pathname-expand (to reflect Chicken 4.10+ behavior)

## [0.10.0] - 2015-10-09

### Added
- google-trends task: Reports top-20 trending searches in the USA. Lots of extra info related to each entry is returned, including estimates of the number of searches (e.g., >500,000)
- google-country-trends: Reports top-20 trending search phrases for every country supported by Google trends
- wikipedia-page-links: Reports links embedded in a given wiki page. Returns the source page, namespace (ns), and the page title of each link
- twitter-followers task: Returns information on all followers of a specified user
- twitter-friends task: Returns information on all friends of a specified user
- tumblr-auth: Tumblr is now accessible! This task sets up the user's credentials for future data pulls.
- tumblr-blog-info: Retrieves blog information (title, description, number of posts, and more) for a given blog. Also accepts a comma-separated list of blog names, fetching each in turn.
- tumblr-posts: Retrieves posts for a given blog, up to any number requested. With a large number, you can easily grab a blog's entire post history. Also accepts a comma-separated list of blog names, fetching each in turn.
- tumblr-tag: Search tumblr by tag (keyword), returning matching posts up to a specified number. Also accepts a comma-separated list of tags, fetching each in turn.

### Changed
- Command line arguments now take precedence over configuration file options
- twitter-user now optionally accepts multiple user names separated by commas. This allows for batch collection of lists of user names

## [0.9.5] - 2015-08-28

### Added
- Required options for each task now listed with the task-options help display
- wikipedia-search task: Returns information about wiki articles containing a given search query
- wikipedia-text task: Returns the full plain text of a requested wiki article
- wikipedia-views task: Returns daily page views for a requested article for a given date range

### Changed
- Increased number of retry attempts for failed/dropped connections to Twitter
- Duration (e.g., for twitter-stream) now controlled via date/time stamps instead of seconds

### Fixed
- twitter-auth bug that prevented new users from authenticating


## [0.9.3] - 2015-07-28

### Changed
- First release using the new Chicken Scheme core

