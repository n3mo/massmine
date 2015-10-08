# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][unreleased]

### Added
- google-trends task: Reports top-20 trending searches in the USA. Lots of extra info related to each entry is returned, including estimates of the number of searches (e.g., >500,000)
- google-country-trends: Reports top-20 trending search phrases for every country supported by Google trends
- wikipedia-page-links: Reports links embedded in a given wiki page. Returns the source page, namespace (ns), and the page title of each link
- twitter-followers task: Returns information on all followers of a specified user
- twitter-friends task: Returns information on all friends of a specified user
- tumblr-auth: Tumblr is now accessible! This task sets up the user's credentials for future data pulls.
- tumblr-blog-info: Retrieves blog information (title, description, number of posts, and more) for a given blog. Also accepts a comma-separated list of blog names, fetching each in turn.

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

