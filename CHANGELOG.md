# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][unreleased]

### Added
- google-trends task: Reports top-20 trending searches in the USA. Lots of extra info related to each entry is returned, including estimates of the number of searches (e.g., >500,000)
- google-country-trends: Reports top-20 trending search phrases for every country supported by Google trends

### Changed
- Command line arguments now take precedence over configuration file options

## [0.9.5] - 2015-08-28

### Changed
- Increased number of retry attempts for failed/dropped connections to Twitter
- Duration (e.g., for twitter-stream) now controlled via date/time stamps instead of seconds

### Fixed
- twitter-auth bug that prevented new users from authenticating

### Added
- Required options for each task now listed with the task-options help display


## [0.9.3] - 2015-07-28

### Changed
- First release using the new Chicken Scheme core

