# MassMine

*WARNING: This project is currently in pre-release form. Day to day changes are likely to break existing functionality. Upcoming version 1.0.0 will mark the first stable release*

![Splash Screen](https://github.com/n3mo/massmine/raw/master/img/splash.png)

MassMine is a research tool designed to simplify the collection and use of data from social media outlets.

Specifically, MassMine:

* Is designed for use on personal computers or on servers/clusters.
* Handles the acquisition of data from sites such as Twitter, Facebook, etc.. 
* Makes setting up custom data requests easy, allowing for flexible deployment for individualized research needs.
* Creates and manages databases of collected data automatically. This means that all tweets, trends, posts, etc. related to your research questions are saved permanently on your local hard drive. 
* Is designed for long-running data collection projects. MassMine makes it easy to create your own database of social media content spanning days, months, or years.
* Balances ease of use with powerful flexibility:
    * Offers full access to social media APIs. 
	* Designed for the non-programmer looking to get into big data research, but offers alternative functionality for power users.
* Runs in several modes:
	1. **Setup**. This mode walks the user through the setup process, creating template files, setting up authorization with social media sites, etc..
    2. **Automated**. This mode makes it easy to run the software on remote servers. MassMine can be set to automatically restart after server crashes to help minimize any loss of data.
    3. **Interactive**. Power users can opt to run MassMine interactively in an already running R session. This allows for full control over the functionality of MassMine, as well as the ability to work with the resulting databases.

## Installation

MassMine is designed to run on Linux and Mac OSX. Windows is currently not supported.

MassMine requires the following dependencies. Each of these must be installed prior to running MassMine:

1. R. The R statistical computing language is freely available for all major operating systems. It is typically quite easy to install. Instructions for doing so can be found on the [The R Project for Statistical Computing](http://www.r-project.org/)
2. cURL. This is a command line utility for fetching web content. It is likely already installed on your system (both Linux and Mac OS X).

If you are using Linux, you may also need to install the cURL development libraries. If you're using Ubuntu (or similar), you can check for this as follows:

Check to see if they're installed already:
```sh
locate libcurl
```

If you see *libcurl.so* somewhere in the output of that command, you should be ready to go. If not, you can install the library with:

```sh
sudo apt-get install libcurl4-openssl-dev
```
