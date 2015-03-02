####################################################################
##
## MassMine: twitter module
## Copyright (C) 2014-2015  Nicholas M. Van Horn & Aaron Beveridge
## 
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## 2014-10-15 NVH: Initial version.
##
## Instructions: This file is a massmine module. It is not meant to be
## ran directly

####################################################################
## API Authentication
####################################################################
mmAuth <- function(config, apiType) {
  ## Handles authentication with Twitter's servers

  ## Currently, the Rest API and streaming API use different
  ## authorization methods. Depending on the task, we use the
  ## corresponding method
  
  if (apiType == "stream") {
    ## The location of pre-authorized twitter credentials on disk. This
    ## file may or may not exist.
    twit_file = config$twitter$twit_file

    ## If the user already has a twitter OAuth object on file, offer
    ## to use that
    if (file.exists(twit_file)) {
      if (!interactivep) {
        resp = "yes"
      } else {
        ## Should we use the OAuth credentials on disk?
        resp = getResponse('\nUse previous Twitter credentials?',
            c("Yes", "No"))
      }
    } else {
      resp = "no"
    }

    ## Now we either load the previous OAuth credentials, or
    ## authenticate with new credentials, according to the user's choice
    if (tolower(resp) == "yes") {
      ## If we're here, things are easy. Just load the old credentials
      ## and register them again
      load(twit_file)
      ## registerTwitterOAuth(twitCred)
    } else {
      ## If we made it here, the user wants to manually authenticate.
      if (!interactivep) {
        cat("\nBefore using MassMine with Twitter, you must",
            "authenticate your Twitter account.",
            "To do this, start R and run the command:",
            "\nsource(\"massmine\")\n",
            "from within the MassMine directory.", sep = "\n")
        stop("Quitting MassMine...", call.=FALSE)
      } 
      resp = getResponse(
          '\nPlease choose an account to authenticate:',
          config$mm_apps)

      myapp = which(resp == config$mm_apps)
      
      consumerKey = config$mm_keys[myapp]
      consumerSecret = config$mm_secrets[myapp]

      accessToken = config$mm_access_tokens[myapp]
      accessSecret = config$mm_access_secrets[myapp]

      ## You need to reauthenticate to the twitter API using oauth:
      reqURL <- "https://api.twitter.com/oauth/request_token"
      accessURL <- "https://api.twitter.com/oauth/access_token"
      authURL <- "https://api.twitter.com/oauth/authorize"

      twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                                   consumerSecret=consumerSecret,
                                   requestURL=reqURL,
                                   accessURL=accessURL,
                                   authURL=authURL)
      twitCred$handshake()
      ## registerTwitterOAuth(twitCred)

      ## Save these credentials for next time. This step allows the
      ## script to start up with manually authenticating on future runs
      save(list="twitCred", file=twit_file)
    }

    ## Regardess of how we authenticated, return the OAuth credentials
    return(twitCred)
  } else {
    ## We're using the Rest API if we've made it here. This uses the
    ## httr method

    myapp = config$mm_apps
    
    consumerKey = config$mm_keys
    consumerSecret = config$mm_secrets

    accessToken = config$mm_access_tokens
    accessSecret = config$mm_access_secrets

    ## Authenticate with oauth using httr
    setup_twitter_oauth(consumerKey, consumerSecret,
                        accessToken, accessSecret)
    }
  
} # End of function mmAuth

mmTest <- function(verbose=FALSE, logfile = stdout()) {
  ## MassMine twitter service tests: tests all available
  ## functionality. Assumes that mmAuth has already authenticated each
  ## requested service, and thus also implicitly tests whether OAuth
  ## authentication is also working correctly.

  ## We use tryCatch functionality for now to avoid requiring another
  ## external dependency like testthat.
  
  ## Verify that we can reach a network connection. This is a silly
  ## hack around, but it gets the job done
  ## out <- is.character(getURL("www.google.com"))

  ## Trend locations
  tryCatch(
    {
      if (!verbose) {sink("/dev/null")}
      loc = getTrendLocations()
      if (!verbose) {sink()}
      stopifnot(is.data.frame(loc) | !is.null(loc))
    },
    error = function(e) {
      if (!verbose) {sink()}
      cat('Task [locations]: FAILED!\n', file = logfile,
          append = TRUE) 
    },
    finally = cat("Task [locations]: success!\n", file = logfile,
      append = TRUE))
  
  ## User timelines. Results are test-saved to temporary files
  tryCatch(
    {

      if (!verbose) {sink("/dev/null")}
      tmpDir1 = tempfile('mm_test')
      tmpDir2 = tempfile('mm_test')
      dir.create(tmpDir1, recursive=TRUE)
      dir.create(tmpDir2, recursive=TRUE)

      ## User list supplied as vector
      fetchUsers(c('twitter'), tmpDir1,
                 numTweets = 201)
      ## User list supplied as text file
      tmpFile = tempfile('mm_user_test.dat',
        tmpdir = tmpDir2)
      write('twitter', file = tmpFile, sep="\n")
      fetchUsers(tmpFile, tmpDir2,
                 numTweets = 201)
      if (!verbose) {sink()}
    },
    error = function(e) {
      if (!verbose) {sink()}
      cat('Task [timeline]: FAILED!\n', file = logfile,
          append = TRUE)
    },
    finally = cat("Task [timeline]: success!\n", file = logfile,
      append = TRUE))

  ## Top 10 trends monitoring
  tryCatch(
    {
      if (!verbose) {sink("/dev/null")}
      monitorTrends(tempfile('mm_trends_test.dat'), samples=1)
      if (!verbose) {sink()}
      stopifnot(is.data.frame(loc) | !is.null(loc))
    },
    error = function(e) {
      if (!verbose) {sink()}
      cat('Task [trends]: FAILED!\n', file = logfile,
          append = TRUE)
    },
    finally = cat("Task [trends]: success!\n", file = logfile,
      append = TRUE))

  ## Streaming API
  tryCatch(
    {
      if (!verbose) {sink("/dev/null")}
      streamFilter(track = "fitness", tweets = 10)
    },
    error = function(e) {
      if (!verbose) {sink()}
      cat('Task [stream]: FAILED!\n', file = logfile,
          append = TRUE)
    },
    finally = cat("Task [stream]: success!\n", file = logfile,
      append = TRUE))

  ## Search RESTful API
  tryCatch(
    {
      if (!verbose) {sink("/dev/null")}
      tmp = searchTweets("fitness")
      if (!verbose) {sink()}
      stopifnot(is.data.frame(tmp), !is.null(tmp))
    },
    error = function(e) {
      if (!verbose) {sink()}
      cat('Task [search]: FAILED!\n', file = logfile,
          append = TRUE)
    },
    finally = cat("Task [search]: success!\n", file = logfile,
      append = TRUE))

  ## We're done.
  cat('\n\nMassMine testing done!\n',
      'Check for any FAILED\n tasks above\n\n',
      file = logfile, append = TRUE)
} # End of function mmTest

getTimeline <- function(user, n=20, includeRts=F, maxID=NULL,
                        sinceID=NULL, ...) { 
  ## This is a simple wrapper for the twitteR function
  ## userTimeline. It's search functionality is indentical, including
  ## the same defaults. The only difference is that the results are
  ## converted to a data frame for you.
  ## Example usage:
  ##   user = getTimeline("ladygaga")

  ## This is the standard call to the twitteR function "userTimeline"
  ## with all possible parameters. Extra parameters are passed to
  ## getURL as usual.
  userData = userTimeline(user, n=n, maxID=maxID, sinceID=sinceID,
    includeRts = includeRts, ...) 
  ## Convert to a data frame and return. This is the only thing that
  ## this getTimeline function does over the twitteR function
  ## userTimeline.
  return(do.call("rbind", lapply(userData, as.data.frame)))
  ## End of function getTimeline
}

plotTimeline <- function(user, breaks = "day", smooth=F,
                         retweets = F, ...) { 
  ## This function plots number of tweets per unit time for a specified
  ## user. Input argument "user" should be a properly-formed data frame
  ## as returned by the function getTimeline (If you want to plot a
  ## subset of tweets, only pass a subset of the data frame "user" to
  ## this function). "breaks" determines how tweets are
  ## binned. Allowable strings include "year", "month", "week", "day",
  ## and "hour". "smooth" is a boolean determining whether
  ## locally-weighted regression (lowess) is performed. Lowess curves
  ## are added in red and provide a quick-and-dirty smoothing of the
  ## data points for visual convenience. "retweets" is a boolean
  ## determing whether or not retweets are included in analysis and
  ## plotting. All remaining optional input arguments "..." are passed
  ## to plot for control over figure generation.
  ## Example usage:
  ##  user = getTimeline("ladygaga", n=1000, includeRts=T)
  ##  plotTimeline(user, retweets=T, breaks="week")

  ## If retweets are unwanted, we remove them now
  if (retweets==F) {
    user = user[user$isRetweet == F, ]
  } 

  ## Convert timestamp of tweet to POSIX date. This shouldn't be
  ## necessary as the data in user$created is already of type POSIXct
  ## user$created = as.POSIXct(user$created)
  
  ## Create a column for days (this could be weeks, hours, etc.). This
  ## gives a time-unit to each tweet
  user$day = cut(user$created, breaks = breaks)
  ## Count number of tweets, binned by day
  tmp = sapply(levels(user$day),
    function(x) dim(user[user$day==x, ])[1]) 
  counts = as.vector(tmp)
  days = as.Date(names(tmp))  # as.Date from zoo library
  
  ## Plot number of tweets by unit of time
  plot(days, counts,
       xlab= paste(toupper(substring(breaks, 1, 1)),
         substring(breaks, 2), sep=""),
       ylab = "Number of tweets", ...)
  if (smooth) {
    lines(lowess(days, counts), col="red")
  }

} ## End of function plotTimeline

searchTweets <- function(searchString, n=25, lang=NULL, since=NULL,
                         until=NULL, locale=NULL, geocode=NULL,
                         sinceID=NULL, retryOnRateLimit=120, ...) { 

  ## This is just a wrapper for the twitteR package function
  ## "searchTwitter". Arguments and their defaults are the same as those
  ## in the function searchTwitter. The results of the search are
  ## converted to a data frame and returned.

  ## This is the standard call to the twitteR package
  tweets = searchTwitter(searchString, n=n, lang=lang, since=since, until=until,
    locale=locale, geocode=geocode, sinceID=sinceID, 
    retryOnRateLimit=retryOnRateLimit, ...)

  ## Convert to a data frame and return. This is the only thing that
  ## this searchTweets function does over the twitteR function
  ## searchTwitter.
  return(do.call("rbind", lapply(tweets, as.data.frame)))
  
} ## End of function searchTweets

restSearch <- function(file.name, query, tweets=25, lang=NULL, since=NULL,
                         until=NULL, locale=NULL, geocode=NULL,
                         sinceID=NULL, retryOnRateLimit=120, ...) {
    ## Wrapper for searchTweets that saves the results to file. This
    ## is meant to be used with massmine's automated functionality
    tmp = searchTweets(searchString=query, n=tweets, lang=lang, since=since,
        until=until, locale=locale, geocode=geocode, sinceID=sinceID,
        retryOnRateLimit=retryOnRateLimit, ...)
    
    saveData(tmp, file.name)
} ## End of function restSearch

sampleUsers <- function(tweets, n=20) {

  ## This function returns a random sample of twitter users from those
  ## users found in the data frame "tweets". The df "tweets" should be
  ## a properly formed df as returned by the function searchTweets. n
  ## is the number of users desired. Note that n <= dim(tweets)[1], or
  ## an error is returned. Several additional things should be kept in
  ## mind. The users returned are only random insofar as they are
  ## randomly chosen from those users present in your data frame. If
  ## "tweets" was prepared using a single keyword, then these users'
  ## activity levels are likely correlated, and are not random in the
  ## sense of a random sample. Also, if your n == dim(tweets)[1], then
  ## the results returned are just tweets$screenName. There's nothing
  ## random about this either.

  ## Throw an error if n > number of tweets supplied and return
  ## nothing
  nTweets = dim(tweets)[1]
  if (n > nTweets) {
    warning("Sample size n > number of tweets")
    return()
  }

  ## Because of multiple tweets from the same user, it's possible that
  ## unique(tweets$screenName) < n. In this case, we return all unique
  ## screen names, but warn the user so they know. Otherwise, we take
  ## random sample of size n from the unique screen names
  users = unique(tweets$screenName)
  if (length(users) <= n) {
    cat(
      sprintf("Too few users present to sample %d. Returning %d unique users instead\n",
              n, length(users)))
    return(users)
  } else {
    return(sample(users, n, replace=FALSE))
  }
} ## End of function sampleUsers

checkUser <- function(user) {
  ## Apply a series of heuristics to a user's time line to determine if
  ## they should be kept or rejected. This function provides an easily
  ## modifiable method for adjusting our criteria for a "good"
  ## user. Argument "user" is a properly formed data frame as returned
  ## by getTimeline. The function returns TRUE if the user passes our
  ## criteria, and FALSE otherwise

  ## Rule 1: Users with ONLY retweets are rejected.
  if (all(user$isRetweet)) {
    return(FALSE)
  }

  ## If we've made it this far, the user passes our criteria. Return
  ## TRUE 
  return(TRUE)
} ## End of function checkUser

hashtags <- function(tweets) {
  ## Returns a list of all hash tags as found in data frame
  ## "tweets". "tweets" should be properly formed, as returned by
  ## getTimeline and searchTweets. All hash tags, including repeats are
  ## returned. You may consider tabulating the frequency of different
  ## hash tags with table(hashtags(tweets)) or reducing to a list of
  ## unique hash tags with unique(hashTag(tweets))

  ## regexp for matching hash tags (POSIX compliant). The following
  ## regexp matches any # character followed by 1 or more characters
  ## except for whitespace or punctuation.
  myregexp = "#[^[:space:]|[:punct:]]+"

  ## Grab all the hash tags from every tweet (including repeats) and
  ## return. 
  unlist(sapply(1:(dim(tweets)[1]),
                function(x) {
                  str_extract_all(tweets$text[x], myregexp)}))


} ## End of function hashtags

hashUsers <- function(texts) {
    ## Similar to hashtags, but uses base R functions. Extracts
    ## hashtags and user names in one pass.
    unlist(regmatches(texts, gregexpr("[#|@][^[:space:]|[:punct:]]+", texts)))
} ## End of function hashUsers

filterTweets <- function(tweets, keyword, ignore.case=F) {
  ## Given a data frame formed by getTimeline or searchTweets, this
  ## returns the subset of tweets that do NOT contain
  ## "keyword". "keyword" can be a string or a vector of strings if you
  ## want to filter out multiple keywords at once. The keywords
  ## themselves can be literal matches (e.g., a hash tag like "#vegan")
  ## or POSIX regular expressions. Case sensitivity for matches is
  ## controlled by the boolean argument ignore.case
  ## Examples:
  ##   ## Find users who tweeted hash tag #vegan
  ##   tweets = searchTweets("#vegan", n=100)
  ##   ## Remove tweets with that hash tag or #vegetarian
  ##   tweets_filtered = filterTweets(tweets, c("#vegan",
  ##                                  "#vegetarian"))
  ##   ## Show a frequency table of remaining hash tags
  ##   table(hashTag(tweets_filtered))

  if (ignore.case) {
    keyword = ignore.case(keyword)
  } 

  ## Find tweets that DO contain any keywords
  idx = sapply(1:(dim(tweets)[1]),
    function(x) {
      any(str_detect(tweets$text[x], keyword))})

  ## Return those tweets that do NOT contain keyword
  tweets[!idx, ]
} ## End of function filterTweets

monitorTrends <- function(file.name, woeid=23424977, samples=1,
                          hashtags=TRUE, logfile = stdout()) {
  ## The following function takes as input a woeid, or a vector of
  ## woeids of length < 6, and grabs the latest twitter trends once
  ## every 5 minutes for "sample" number of times for the given
  ## woeid. Each 5-minute's worth of results are given a timestamp and
  ## then written (appended) to the file "file.name". Provided the
  ## function runs without error for "samples" times, all data from all
  ## searches are written to file. In the case of an error (for any
  ## reason), the current sample's data are lost, but the process will
  ## continue running, attempting to collect subsequent trend
  ## data. Data are written along the way, so a cataclysmic error
  ## won't result in a data loss of previously collected data.

  ## Include hashtags in trends?
  if (hashtags == FALSE | tolower(hashtags) == "false") {
    exclude = "hashtags"
  } else {
    exclude = NULL
  }

  ## If the user used a YAML config file to submit multiple WOEIDS,
  ## the ids will be encoded as a string. We must split the string
  ## into separate numbers and convert to numeric. This regex (and
  ## splitting technique) allows the woeids in the YAML file to be
  ## separated by any combination of spaces (or tabs, etc.) AND/OR
  ## punctuation. 
  if (is.character(woeid)) {
    tmp = unlist(strsplit(woeid, split = "[[:space:][:punct:]]"))
    woeid = as.numeric(tmp)[nchar(tmp) != 0]
    rm(tmp)
  }

  ## No more than 5 woeids can be fetched in a 5 minute period without
  ## violating our rate limits
  N_woeid = length(woeid)
  if (N_woeid > 5) {
    stop("5 woeids max allowed", call.=FALSE)
  }

  ## Don't allow the user to overright or append to an existing
  ## file. There should be no way to bypass this check. The user (or
  ## calling function) should give a new file name every time
  ## monitorTrends is called to prevent accidental data loss.
  if (file.exists(file.name)) {
    stop(sprintf("File %s already exists", file.name), call.=FALSE)
  } 

  cat(sprintf("Collecting %d top-10 trend lists for %d woeid(s)\n",
              samples, N_woeid), file = logfile, append = TRUE)
  cat("Wait duration is 5 minutes between samples\n\n",
      file = logfile, append = TRUE)
  
  ## We define some helper functions for writing data to file and
  ## for error handling

  ## This retrieves the latest trend data for the supplied woeid and,
  ## if successful, writes (by appending) a sample's worth of data to
  ## file, adding a header of column names when sampleNum ==
  ## 1. datestamp is added to the data as a column. Header is a
  ## boolean that should only be set to TRUE on the very first sample
  ## of the very first woeid. When TRUE, it writes a header of column
  ## names to the data file (and it does NOT append-- it destroys the
  ## file's contents!)
  handleTrend <- function(mywoeid, sampleNum, header,
                          logfile = stdout()) {

    ## This call requests the latest trend data. If this fails, we
    ## should kick out and the calling function is responsible for
    ## handling the error...
    d = getTrends(mywoeid, exclude=exclude)
    ## simpleError("Debug error")   # Error handling debugging

    ## If the previous call returned an error, we should have already
    ## fallen out of this function back into the calling function
    ## (which should be handling the error). It is possible that the
    ## call to getTrends from above did NOT return an error, but did
    ## return a malformed data frame (or some other unpredictable
    ## data). Here we double check that the data appears to of the
    ## correct type, signaling an error if something looks wrong. We
    ## do this to avoid corrupting the file.name that we're writing to
    ## below.
    if (class(d) != "data.frame") {
      stop("Malformed trend data frame", call.=FALSE)
    } 

    ## Add the current timestamp to each row. We use the actual
    ## current time, rather than the time of the get request
    ## getTimes[i] in case twitter's API took a long time return.
    d$time = Sys.time()

    ## The data return are rank ordered 1-10. Here we give each
    ## trend its corresponding rank number for the current
    ## data. We should get 10 trends on every call, but the
    ## following code should work for any list >= 1.
    d$rank = 1:(dim(d)[1])

    ## Notify the user
    cat(sprintf("Sample %d of %d for woeid %d... ", sampleNum,
                samples, mywoeid), file = logfile, append = TRUE)
    
    ## Write the latest results to file and merge with previous
    ## data. We only write the column names (the header) on the
    ## first pass.
    if (header) {
      ## WARNING: This call overwrites the contents of file.name!
      ## This shouldn't be a problem, as the beginning of this
      ## function stops if the file already exists.
      write.table(d, file=file.name, sep=",",
                  row.names=F, append=FALSE, col.names=TRUE)
      ## Create new data frame for storing the results away
      ## As of 2013-11-17, we longer keep the data from in
      ## d. The logic is that if monitorTrends is left running
      ## for months, then allData could go too large and crash
      ## R. Plus, the data are written to file and are typically
      ## read and analyzed by a separate instance of R, so
      ## saving the data in allData serves no purpose.
      ## allData = d
    } else {
      write.table(d, file=file.name, sep=",",
                  row.names=F, append=T, col.names=FALSE)
      ## Add current trend results to previous results
      ## As of 2013-11-17 this data is no longer saved within
      ## R. It is only written to file. 
      ## allData = rbind(allData, d)
    }

    cat("saved... ", file = logfile, append = TRUE)
    ## End of function handleTrend
  }

  ## Error handling for situations in which the request to twitter's
  ## API times out, returns mal-formed json, or whatever
  handleError <- function(sampleNum, mywoeid, logfile = logfile) {
    cat(sprintf("An error occured on sample %d for woeid %d... ",
                sampleNum, mywoeid), file = logfile, append = TRUE)
  }
  
  ## Grab trend data once for each time stamp in getTimes. Don't
  ## request the data too early or we will violate our rate limit of
  ## 5 calls per 15 minutes. Setting our waiting period should
  ## protect us from this mistake (as the actual limit is at 1 call
  ## per 3 minutes). Plus, the trend data are cached every 5
  ## minutes, which makes a call of < 5 minutes useless anyways.

  ## The main loop, one iteration for each sample
  for (i in 1:samples) {

    ##  Here, we wait for 5 minutes (300 seconds) before moving on
    ## to the next request. For the first request, we don't wait
    ## at all. This method sets R's system priority to low,
    ## freeing up your resources while R waits.
    if (i > 1) {
      Sys.sleep(300)
      ## Sys.sleep(5)  # Debug only!
    }
    
    ## Finally, we get down to business. The strategy is as
    ## follows: (1) Request the latest top-10 trends through
    ## twitter's API. If successful, then (2) write the results to
    ## our output file, then repeat the process. On error, (2)
    ## print a message indicating the error, then repeat the
    ## process. That is, errors in retrieving data from twitter
    ## should not cause monitorTrends to crash completely. The
    ## data for the current 5-minute interval are lost, however. 

    ## We now fetch the data for each woeid supplied
    for (j in 1:N_woeid) {

      ## We create a header line of column names on the first sample in
      ## the data file
      doHeader = ifelse(i==1 & j==1, TRUE, FALSE)

      tryCatch(
        handleTrend(woeid[j], i, doHeader),
        error = function(e) handleError(i, woeid[j], logfile = logfile),
        finally = cat("done.\n"), file = logfile, append = TRUE)
    }

    ## Done with trend fetching for the current time sample.

  }

} ## End of function monitorTrends

fetchUsers <- function(users, outdir, tweets = 3200, logfile =
                       stdout()) {
  ## Wrapper for getTimeline. This function provides an automated
  ## method for harvesting multiple user's timelines from twitter's
  ## API. Given a character vector of users "users", fetchUsers will
  ## fetch for each user the minimum of either their full timeline, or
  ## their previous 3200 statuses (the oldest allowable tweet by
  ## twitter's API). Each user's data is written to an individual file
  ## in the directory "outdir". User data file names will be generated
  ## according to the pattern user_SCREENNAME.csv, where SCREENNAME is
  ## the user's official twitter screen name. Additionally, two files
  ## will be created. One file contains screen names of users
  ## successfully fetched (fetched_users_XXXXX.txt) and another for
  ## users that led to fetching failures (failed_users_XXXXX.txt). In
  ## both cases the XXXXX will be replaced with a unique random number
  ## not found in the directory "outdir". To avoid exceeding rate
  ## limits, fetchUsers tries to monitor the number of requests and
  ## time limit windows. At 200 statuses per request, this means that
  ## each user could require 16 separate API requests (even though in
  ## practice, many users will have < 3200 statuses). This
  ## conservative assumption means that the strict limit of 180
  ## requests per 15 minutes dictates that we fetch no more than 11
  ## users per 15 minute interval if each user has 3200+ tweets in
  ## their timeline: floor(180 / (3200 / 200). But because many users
  ## will have <3200 total tweets, fetchUsers will conservatively
  ## estimate the number requests used per user, squeezing in
  ## additional requests per limit window whenever possible.
  ## fetchUsers automatically monitors timing, pausing when necessary
  ## to ensure rate limits are observered. Error handling ensures that
  ## failures do not disrupt the entire fetching process. User
  ## requests that fail for any reason are printed to stndout. Errors
  ## cause the user to be skipped. No data is saved for the user in
  ## question.
  ##
  ## As a convenience, instead of a character vector, "users" can be a
  ## file path (e.g., "~/path/to/user_list.dat"). If a file path is
  ## supplied, it is assumed to have one user screen name, unquoted, per
  ## line. That is, the file should contain screen names as plain text
  ## separated by the newline character "\n"

  ## Rate limits. Number of requests per limit window
  rateLimit = 180
  ## Limit window duration in MINUTES
  rateWindow = 15
  ## Because the API returns up to 200 statuses per request and 3200
  ## statuses max, a single user will take up to 16 requests, so we
  ## should always assume they could take this many
  statusPerRequest = 200
  maxRequestsPerUser = 16

  ## For purposes downstream, we don't want outdir to end with a slash
  ## "/". We remove it here if present.
  if (substr(outdir, nchar(outdir), nchar(outdir)) == "/") {
    outdir = substr(outdir, 1, nchar(outdir)-1)
  } 

  ## The output directory for to-be-created user data files must
  ## exist! 
  if (!file.exists(outdir)) {
    stop(sprintf("Directory %s does not exist!\n", outdir),
         call.=FALSE) 
  } 

  ## If the user supplied a file path, we read in the user list from
  ## disk 
  if (file.exists(users[1])) {
    users = scan(users, what = character())
  }
  
  ## We only want unique users
  users = unique(users)
  N_users = length(users)
  ## We will keep track of the number of successes (user timeline
  ## requests that did NOT result in error for any reason). The screen
  ## names of errors will be written to one file, and the screen names
  ## of successes will be written to another file. The log file names
  ## are randomly generated below. Log files are saved in outdir
  N_successes = 0
  while (TRUE) {
    successFile = paste(outdir, "/fetched_users_",
      round(runif(1, 10000, 99999)), ".txt", sep = "")
    failureFile = paste(outdir, "/failed_users_",
      round(runif(1, 10000, 99999)), ".txt", sep = "")
    ## Only keep the file names if they don't already exist
    if (!file.exists(successFile) & !file.exists(failureFile)) {
      break
    } 
  }
  
  ## Estimated duration in hours. 11 users per 15 minutes (max)
  maxDur = ((N_users/11) * rateWindow) / 60
  
  ## Useful diagnostics
  cat(sprintf("Fetching %d user time lines...\n", N_users),
      sprintf("Estimated maximum run time of %0.2f hours\n", maxDur),
      file = logfile, append = TRUE)

  ## Timing methods. We cannot exceed the rate limit of twitter's
  ## API. This keeps track of the number of calls made. It is reset
  ## below every 15 minutes.
  rateRequests = 0

  ## Time used in MINUTES. This starts at zero and increments
  ## according to real time as we loop below. If the number of actual
  ## requests exceeds our rate limit in 15 minutes, then a delay is
  ## imposed. Once 15 minutes is up, rateRequests and totalTime are
  ## reset to 0.
  totalTime = 0

  ## Let's get down to business
  for (i in 1:N_users) {

    ## Begin message. Finished below in one way or another...
    cat(sprintf("User %s (%d of %d): ", users[i], i, N_users),
        file = logfile, append = TRUE)

    ## Start timer for this loop
    startTime = Sys.time()
    
    ## Error handling
    tryCatch(
      ## We try to do this (get the current user's data and write it
      ## to disk.
      {
        ## stop("Debug error", call.=FALSE) # Debug
        
        ## Let's determine the current user's file that we will
        ## create. 
        userFile = file.path(outdir,
          paste("user_", users[i], ".csv", sep=""))

        ## If the file userFile already exists, we signal an error
        ## rather than processing the current user. The reason is
        ## two-fold. (1) Fetching data is time consuming. There is
        ## no reason to fetch data that we already have. (2) This
        ## function should not have the power to overwrite existing
        ## data. We don't want to lose any previous data
        ## accidentally.
        if (file.exists(userFile)) {
          stop(sprintf("User file %s already exists.",
                       basename(userFile)))
        } 

        ## This does all the heavy lifting. We request tweets
        ## tweets (the maximum-allowed look into the past). We may get
        ## far less for less active users.
        userTweets = getTimeline(users[i], n=tweets, includeRts=T)

        ## If the returned data frame has 0 rows, or is not a data
        ## frame (due to any error), then we do not write to
        ## file. We signal an error and move on to the next user
        ## without doing anything further. The user is skipped for
        ## good.
        if (dim(userTweets)[1] == 0 |
            class(userTweets) != "data.frame") { 
          stop(sprintf("Data missing or malformed.",
                       users[i]))
        }

        ## Everything looks good. Save the data and finish up.
        saveData(userTweets, userFile, append=FALSE)
        ## Add the user name to our successes file
        write(users[i], successFile, sep="\n", append=TRUE)
        ## Increment success counter
        N_successes = N_successes + 1
        cat(" ... ", file = logfile, append = TRUE)

        ## End of try expression
      },
      ## Error handling
      error = function(e) {
        cat(e$message, " Skipping... ", file = logfile, append = TRUE)
        ## Add the user name to our failures file
        write(users[i], failureFile, sep="\n", append=TRUE)

      },
      ## Finish up tryCatch with a confirmation message
      finally = cat("done.\n", file = logfile, append = TRUE))


    ## Update rate and time usage information. You get a maximum of
    ## 200 tweets per call, so we add an appropriate number of request
    ## estimates, always rounding up
    if (!exists("userTweets")) {
      ## There was an error above. Assume we used the maximum number
      ## of requests for this user
      rateRequests = rateRequests + maxRequestsPerUser
    } else {
      rateRequests = rateRequests +
        ceiling(dim(userTweets)[1] / statusPerRequest)
    }

    ## We no longer need the data frame of tweets-- we remove it
    ## here. This step is important because on error the script above
    ## checks for the existence of this variable. We don't want a
    ## previous instance of this variable affecting the current
    ## iteration.
    if (exists("userTweets")) {
      rm(userTweets)
    } 

    ## Update time used so far
    totalTime = totalTime +
      as.numeric(difftime(Sys.time(), startTime, units="mins"))

    ## We must check now if our number of requests has exceeded the
    ## rate limit for this 15 minute interval. If so, we must impose a
    ## delay until the 15 minute interval is up. Also, if the 15
    ## minute window is up, we can reset our total time and
    ## rateRequests counters.
    if (totalTime < rateWindow) {
      if ((rateRequests + maxRequestsPerUser) >= rateLimit) {
        ## We're in danger of exceeding our rate limit. We must wait
        ## until our current limit window has expired.
        waitTime = round((rateWindow - totalTime) * 60)
        cat(sprintf("Rate limit exceeded. Waiting %d seconds... ",
                    waitTime), file = logfile, append = TRUE)
        Sys.sleep(waitTime)
        cat("done!\n", file = logfile, append = TRUE)

        ## We're done waiting. We can now safely reset our counters
        ## and proceed with data collection
        totalTime = 0
        rateRequests = 0
      }

      ## IMPLICIT ELSE: if we've made it here, then there is still
      ## time left in this limit window AND we still have available
      ## requests left. We can proceed on to the next user.

    } else {
      ## If we've made it here, our window has passed before we used
      ## up our rate limit. We can reset our counters and continue
      ## collecting data at full speed
      totalTime = 0
      rateRequests = 0
    }
    ## End of main loop
  }

  ## Print final summary information
  cat("\nUser data fetching complete\n",
      sprintf("%d of %d users successfully fetched\n",
              N_successes, N_users), file = logfile, append = TRUE)


} ## End of function fetchUsers

trendSummary <- function(file) {
  ## Provides a quick summary of trend data collected using
  ## monitorTrends. "file" is a text file created by the function
  ## monitorTrends. 

  ## Read in the data 
  d = read.table(file=file, sep=",", header=T)

  ## Convert the time column into POSIXct class for working with time
  ## stamps 
  d$time = as.POSIXct(d$time)

  ## Number of top-10 samples taken
  N_samples = length(unique(d$time))

  ## Print some useful summaries about data file
  cat(sprintf("\nData file contains %d samples\n",
              N_samples)) 
  cat(sprintf("Trend data begin on %s\n", min(d$time)))
  cat(sprintf("Trend data end on %s\n", max(d$time)))
  print(max(d$time) - min(d$time))

  ## Trend information (what we really want)

  ## Unique trends
  trends = unique(d$name)
  N_trends = length(trends)

  ## Determine average rank of each unique trend
  tmp = t(sapply(trends, function(x) {

    ## Unique time samples containing the current trend. That is, in
    ## all of our sample (5 minutes between each sample, typically),
    ## how many of those samples contained this trend at any spot in
    ## the top ten
    N_top = length(unique(d$time[d$name == x]))

    ## What was the average rank of the current trend across all
    ## samples that it occurred in?
    rank = mean(d$rank[d$name== x])

    cbind(N_top, rank)
  }))

  ## Set up a new data frame using our reduced data
  tData = data.frame(name = trends, N_samples = tmp[,1], rank =
    tmp[,2])

  ## Determine what percentage of total time each trend spent in the
  ## top ten
  tData$top = round((tData$N_samples / N_samples) * 100, 1)

  ## Let's print out some results for the user
  cat(sprintf("%d unique trends identified\n\n", length(trends)))
  
  cat("Top 5 trends by % time spent in top 10:\n")
  cat("=========================================\n")
  print(tData[rev(order(tData$top)),][1:5,])

  cat("\nTop 5 trends by average rank:\n")
  cat("=============================\n")
  print(tData[order(tData$rank),][1:5,])

} ## End of function trendSummary

streamFilter <- function(file.name="", track=NULL, follow=NULL,
                         locations=NULL, lang=NULL, timeout=0,
                         tweets=NULL, oauth=twitCred, verbose=TRUE,
                         logfile = stdout(), shrink=FALSE) {
  ## Adapted from filterStream in the streamR package. This does the
  ## heavy lifting for targeting twitter's streaming API. This will be
  ## the new streamFilter function that replaces the original one
  ## below. It attempts to replace the need for one of our library
  ## dependencies 

  buildArgList <- function(track=NULL, follow=NULL, language=NULL, locations=NULL,
                           with=NULL,replies=NULL, oauth=NULL) {
    ## Helper function for creating a list of parameters
    params <- list()
    if (!is.null(track)) params[["track"]] <- paste(track, collapse=",")
    if (!is.null(follow)) params[["follow"]] <- paste(as.character(follow), collapse=",")
    if (!is.null(locations)) params[["locations"]] <- paste(as.character(locations), collapse=",")
    if (!is.null(language)) params[["language"]] <- paste(as.character(language), collapse=",")
    if (!is.null(with)) params[["with"]] <- paste(as.character(with), collapse=",")
    if (!is.null(replies)) params[["replies"]] <- paste(as.character(replies), collapse=",")
    return(params)
  }
  
  ## building parameter lists
  params = buildArgList(track, follow, lang, locations, oauth=oauth)

  ## Tweet counter
  i = 0

  if (verbose==TRUE) message("Capturing tweets...")

  ## The data collected from twitter are in json format. They are
  ## written to disk as we go along for safety purposes, but are
  ## ultimately converted to a csv file. We write the json to disk and
  ## operate on it only after it's successfully saved. This is the raw
  ## json file:
  json_outfile = paste(file_path_sans_ext(file.name), ".json", sep="")

  ## Verify that the data file does not already exist. We don't want
  ## to overwrite anything... (WE NOW USE APPEND=TRUE BELOW)
  ## if (file.exists(file.name) | file.exists(json_outfile)) {
  ##   stop("Output file already exists", call.=FALSE)
  ## } 

  conn <- file(description=json_outfile, open="a")
  write.tweets <- function(x){
    ## writes output of stream to a file
    if (nchar(x)>0) {
      i <<- i + 1
      writeLines(x, conn, sep="")
    }
  }
  if (!is.null(tweets) && is.numeric(tweets) && tweets>0){	
    write.tweets <- function(x){	
      if (i>=tweets){break}	
      ## writes output of stream to a file
      if (nchar(x)>0) {	
        i <<- i + 1	
        writeLines(x, conn, sep="")	
      }	
    }
  } 

  init = Sys.time()
  ## Connect to Streaming API
  url = "https://stream.twitter.com/1.1/statuses/filter.json"
  output =
    tryCatch(
      oauth$OAuthRequest(URL=url, params=params, method="POST",
                         customHeader=NULL, timeout = timeout,
                         writefunction = write.tweets,
                         cainfo=system.file("CurlSSL", "cacert.pem",
                           package = "RCurl")), 
      error=function(e) e)

  ## Close the file we've been writing to
  close(conn)

  ## Information messages
  seconds <- round(as.numeric(difftime(Sys.time(), init, units="secs")),0)
  
  ## Let the user know how things went. 
  if (verbose==TRUE) {
    message("Connection to Twitter stream was closed after ",
            seconds,  
            " seconds with up to ", i, " tweets downloaded.")
  }

  ## IMPORTANT UPDATE: As of 2015-03-02, MassMine exports data in JSON
  ## format by default. This (1) retains ALL of the data returned by
  ## Twitter, whatever it becomes in the future, (2) avoids complex,
  ## error-prone on-the-fly processing json simplifies data collection
  ## and, separates data collection from data filtering. The data
  ## processor json2csv can handle data filtering for arbitrarily
  ## large JSON files after collection is safely complete.

  ## ## Convert the resulting json file into a data frame, and then save
  ## ## as a csv file
  ## ## Read the text file and save it in memory as a list
  ## lines <- suppressWarnings(readLines(json_outfile, encoding="UTF-8"))

  ## results.list <- lapply(lines[nchar(lines)>0], function(x)
  ##                        tryCatch(fromJSON(x), error=function(e) e)) 
  ## ## removing lines that do not contain tweets or were not properly parsed
  ## errors <- which(unlist(lapply(results.list, length))<18)
  ## if (length(errors)>0){
  ##   results.list <- results.list[-errors]
  ## }
  ## ## information message
  ## if (verbose==TRUE) cat(length(results.list),
  ##                        "tweets have been parsed.", "\n")

  ## ## Convert to data frame
  ## ## if no text in list, change it to NULL
  ## if (length(results.list)==0){
  ##   stop(deparse(substitute(tweets)), " did not contain any tweets. ",
  ##        "")
  ## }
  ## ## constructing data frame with tweet and user variable
  ## if (shrink) {
  ##     df <- data.frame(
  ##         text = unlistWithNA(results.list, 'text'),
  ##         retweet_count = unlistWithNA(results.list, c('retweeted_status', 'retweet_count')),
  ##         favorited = unlistWithNA(results.list, 'favorited'),
  ##         in_reply_to_screen_name = unlistWithNA(results.list, 'in_reply_to_screen_name'),
  ##         retweeted = unlistWithNA(results.list, 'retweeted'),
  ##         created_at = unlistWithNA(results.list, 'created_at'),
  ##         lang = unlistWithNA(results.list, 'lang'),
  ##         location = unlistWithNA(results.list, c('user', 'location')),
  ##         user_id_str = unlistWithNA(results.list, c('user', 'id_str')),
  ##         description = unlistWithNA(results.list, c('user', 'description')),
  ##         favourites_count = unlistWithNA(results.list, c('user', 'favourites_count')),
  ##         time_zone = unlistWithNA(results.list, c('user', 'time_zone')),
  ##         screen_name = unlistWithNA(results.list, c('user', 'screen_name')),
  ##         stringsAsFactors=F)
  ##   } else {
  ##       df <- data.frame(
  ##           text = unlistWithNA(results.list, 'text'),
  ##           retweet_count = unlistWithNA(results.list, c('retweeted_status', 'retweet_count')),
  ##           favorited = unlistWithNA(results.list, 'favorited'),
  ##           truncated = unlistWithNA(results.list, 'truncated'),
  ##           id_str = unlistWithNA(results.list, 'id_str'),
  ##           in_reply_to_screen_name = unlistWithNA(results.list, 'in_reply_to_screen_name'),
  ##           source = unlistWithNA(results.list, 'source'),
  ##           retweeted = unlistWithNA(results.list, 'retweeted'),
  ##           created_at = unlistWithNA(results.list, 'created_at'),
  ##           in_reply_to_status_id_str = unlistWithNA(results.list, 'in_reply_to_status_id_str'),
  ##           in_reply_to_user_id_str = unlistWithNA(results.list, 'in_reply_to_user_id_str'),
  ##           lang = unlistWithNA(results.list, 'lang'),
  ##           listed_count = unlistWithNA(results.list, c('user', 'listed_count')),
  ##           verified = unlistWithNA(results.list, c('user', 'verified')),
  ##           location = unlistWithNA(results.list, c('user', 'location')),
  ##           user_id_str = unlistWithNA(results.list, c('user', 'id_str')),
  ##           description = unlistWithNA(results.list, c('user', 'description')),
  ##           geo_enabled = unlistWithNA(results.list, c('user', 'geo_enabled')),
  ##           user_created_at = unlistWithNA(results.list, c('user', 'created_at')),
  ##           statuses_count = unlistWithNA(results.list, c('user', 'statuses_count')),
  ##           followers_count = unlistWithNA(results.list, c('user', 'followers_count')),
  ##           favourites_count = unlistWithNA(results.list, c('user', 'favourites_count')),
  ##           protected = unlistWithNA(results.list, c('user', 'protected')),
  ##           user_url = unlistWithNA(results.list, c('user', 'url')),
  ##           name = unlistWithNA(results.list, c('user', 'name')),
  ##           time_zone = unlistWithNA(results.list, c('user', 'time_zone')),
  ##           user_lang = unlistWithNA(results.list, c('user', 'lang')),
  ##           utc_offset = unlistWithNA(results.list, c('user', 'utc_offset')),
  ##           friends_count = unlistWithNA(results.list, c('user', 'friends_count')),
  ##           screen_name = unlistWithNA(results.list, c('user', 'screen_name')),
  ##           stringsAsFactors=F)

  ##       ## adding geographic variables and url entities
  ##       df$country_code <- unlistWithNA(results.list, c('place', 'country_code'))
  ##       df$country <- unlistWithNA(results.list, c('place', 'country'))
  ##       df$place_type <- unlistWithNA(results.list, c('place', 'place_type'))
  ##       df$full_name <- unlistWithNA(results.list, c('place', 'full_name'))
  ##       df$place_name <- unlistWithNA(results.list, c('place', 'place_name'))
  ##       df$place_id <- unlistWithNA(results.list, c('place', 'place_id'))
  ##       place_lat_1 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 1, 2))
  ##       place_lat_2 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 2, 2))
  ##       df$place_lat <- sapply(1:length(results.list), function(x)
  ##           mean(c(place_lat_1[x], place_lat_2[x]), na.rm=TRUE))
  ##       place_lon_1 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 1, 1))
  ##       place_lon_2 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 3, 1))
  ##       df$place_lon <- sapply(1:length(results.list), function(x)
  ##           mean(c(place_lon_1[x], place_lon_2[x]), na.rm=TRUE))
  ##       df$lat <- unlistWithNA(results.list, c('geo', 'coordinates', 1))
  ##       df$lon <- unlistWithNA(results.list, c('geo', 'coordinates', 2))
  ##       df$expanded_url <- unlistWithNA(results.list, c('entities', 'urls', 1, 'expanded_url'))
  ##       df$url <- unlistWithNA(results.list, c('entities', 'urls', 1, 'url'))

  ##   }
  ## ## retweet_count is extracted from retweeted_status. If this is not a RT, set to zero
  ## df$retweet_count[is.na(df$retweet_count)] <- 0

  ## ## Write the resulting data frame to disk
  ## saveData(df, file.name, append=TRUE)

} ## End of function streamFilter

unlistWithNA <- function(lst, field){
  ## Helper function for streamFilter. This was taken verbatim from
  ## the streamR package (https://github.com/pablobarbera/streamR)
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3 & field[1]!="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (field[1]=="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]]))
  }
  if (length(field)==4 && field[2]!="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]][[field[3]]][[field[4]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]][[field[4]]]))
  }
  if (length(field)==4 && field[2]=="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (length(field)==6 && field[2]=="bounding_box"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x)
                                    x[[field[1]]][[field[2]]][[field[3]]][[as.numeric(field[4])]][[as.numeric(field[5])]][[as.numeric(field[6])]]))
  }
  return(vect)
}



## streamFilter <- function(
##   file.name = "", track = NULL, follow = NULL, locations = NULL,
##   language = NULL, timeout = 0, tweets = NULL, oauth = twitCred,
##   logfile = stdout(), verbose = TRUE) { 
##   ## This is a wrapper function for the streamR library function
##   ## "filterStream". It's purpose is three-fold: (1) To provide a
##   ## chance to rename the function according to our naming
##   ## conventions, and (2) to change certain default parameters. Most
##   ## notably, we insert our twitter credentials as default, so the
##   ## user doesn't have to manage this step. Also, we suppress output
##   ## from streamR functions so that we can control what is printed to
##   ## screen. And (3), we automatically convert the results to a data
##   ## frame using streamR's parseTweets function. This final data frame
##   ## is returned.

##   ## Message the user about capturing tweets
##   ## cat('\nCapturing tweets...\n')

##   ## Allow for multiple keywords in search
##   if (!is.null(track)) {
##     track = unlist(strsplit(track, ","))
##   }

##   ## Ensure that timeout is numeric (changing this in the massmine
##   ## config file will turn it into a string)
##   timeout = as.numeric(timeout)

##   ## If file.name is empty, we create a temporary file for temporarily
##   ## storing our data while it's being collected. If no file is
##   ## supplied, this temporary file is parsed when finished, and the
##   ## resulting data frame is returned
##   if (file.name == "") {
##     tempFile = tempfile('mm_stream_data')
##   } else {
##     tempFile = file.name
##   }

##   ## Start catching tweets
##   d = filterStream(file.name = tempFile, track = track,
##     follow = follow, locations = locations, language = language,
##     timeout = timeout, tweets = tweets, oauth = oauth,
##     verbose = verbose)
  
##   ## Message user about conversion process
##   ## cat('Converting results to data frame... ',
##   ##   file = logfile, append = TRUE)

##   ## Convert the data to a more useful data frame
##   d = parseTweets(tempFile)

##   ## If the user did NOT supply a file name, then we have the raw
##   ## results stored in d. We convert it to a data frame and return
##   if (file.name == "") {
##     return(d)
##   } else {
##     ## Else we write the data to file and return nothing
##     saveData(d, filename = file.name, logfile = logfile, append=FALSE)
##     return()
##   }
  
## } ## End of function streamFilter

getTrendLocations <- function(file.name="") {
    loc = availableTrendLocations()
    ## Returns a data frame of available trends locations with three
    ## columns, name, country, and WOEID. If file.name is non-empty,
    ## the results are written to file.

    if (file.name == "") {
        return(loc)
    } else {
        saveData(loc, filename = file.name)
    }
} ## End of function getTrendLocations

####################################################################
## AUTO-RUN CODE
####################################################################
## Everything from here downward is evaluated automatically whenever
## this file is sourced. Sourcing this file should occur from within a
## running massmine process... sourcing this manually will likely lead
## to errors.

## Authenticate with Twitter
twitCred = mmAuth(config, config$twitter$task)
 

