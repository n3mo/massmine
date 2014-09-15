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

MassMine is designed to run on Linux, Mac OSX, and Windows.

### Dependencies

MassMine requires the following dependencies. Each of these must be installed prior to running MassMine:

1. **R**. The R statistical computing language is freely available for all major operating systems. It is typically quite easy to install. Instructions for doing so can be found on the [The R Project for Statistical Computing](http://www.r-project.org/). MassMine is developed to run on R version 3.0.x. It is likely to also run on newer versions of R, but you may (or may not) run into trouble with older 2.x versions of R.
2. **cURL**. This is a command line utility for fetching web content. It is likely already installed on your system (both Linux and Mac OS X).

If you are using Linux, you may also need to install the cURL development library. If you're using Ubuntu (or similar), you can check for this as follows:

Check to see if it's installed already:
```sh
locate libcurl | grep libcurl.so
```

If you see *libcurl.so* somewhere in the output of that command, you should be ready to go. If not, you can install the library with:

```sh
sudo apt-get install libcurl4-openssl-dev
```

### Install Using Git

If you don't already have git installed, you can do so easily. On Ubuntu, you can install git through the Software Center, or via a terminal:

```sh
sudo apt-get install git
```

For Mac OS X, you can use the latest [git installer](http://sourceforge.net/projects/git-osx-installer/)

Once git is installed, open a terminal and install with:

```sh
cd ~/change/to/path/of/your/choice
git clone https://github.com/n3mo/massmine.git
```

This will create a directory "massmine" in the folder ~/change/to/path/of/your/choice with the necessary files inside.

### Install From Source (Alternative Method To Git)

As an alternative to git, you can simply download a [zip file](https://github.com/n3mo/massmine/zipball/master) or [tarball](https://github.com/n3mo/massmine/tarball/master) of the source code. Extract the compressed file to create a directory "massmine."

### Completing The Installation

Once you have installed the source code (using either method from above), you can setup MassMine by running it.

To begin, make the massmine script executable, and then run it:

```sh
cd ~/change/to/path/of/your/choice/massmine
chmod +x massmine
./massmine
```

On its first run, MassMine will offer to create a user customization template file. The template file will be created in your home folder as `~/.config/massmine/mm_config` (note that files that begin with "." will not show up in your file browser by default. You may need to turn on "hidden files" to see the directory "~/.config").

The template file `~/.config/massmine/mm_config` contains instructions for how to edit it. You will find information for how to include your authorization credentials, etc. Once you have finished editing the customization file, you can rerun massmine at any time to begin using it:

```sh
~/path/to/massmine
```
