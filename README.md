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

On its first run, MassMine will offer to install any required R packages. You must either allow MassMine to install these when prompted, or alternatively you can install them yourself from within R (experienced users only). Once complete, MassMine is installed and is ready to be used. Restart MassMine with `./massmine` to begin using it.

### Configuration File

You control MassMine's behavior with user configuration files. These files are [YAML](http://en.wikipedia.org/wiki/YAML) formatted (that is, they are structured for a computer to read, but are also human-readable). Example configuration files are included with MassMine in the "examples" folder of your installation.

When started, you can specify a custom configuration file by including the file name after the call to MassMine. Assuming you have a file called "my-massmine-config" in the same directory you are working in, you can start MassMine as follows:

```sh
./massmine my-massmine-config
```

If you do not specify a configuration file in this manner, MassMine will search for a configuration file called "mmconfig" in the following locations, in this order:

1. In the current directory that you're calling MassMine in.
2. In the "examples" folder of the MassMine installation, wherever it was installed on your computer.

If no file can be found in these locations, MassMine stops with an error.

### Authenticating With Twitter

If you are using the Twitter functionality of MassMine, you must authenticate with the Twitter servers. This is a requirement of Twitter. To do so, you must log in to (or create) an account at [Twitter's Developer Site](https://dev.twitter.com). Once logged in, you must create a new "application" at [Twitter's Application Site](https://apps.twitter.com/). In your configuration file, edit the entries for "key" and "secret" to include the corresponding codes provided to you by Twitter. You should also provide a name for your Twitter account in your configuration file. 

When you attempt to use MassMine to access Twitter for the first time, you will be asked to authenticate your account. In a terminal, you must enter the following commands:

```sh
cd ~/path/to/massmine
R
```

This will start an R process. At the R prompt, enter:

```R
source("massmine")
```

When you enter the `source("massmine")` command, you will be prompted with an authentication process. Follow the provided web link and enter the numeric code provided to you. Upon success, MassMine will be ready for use with Twitter. You can now quit R with `q()` (choosing "no" when asked about saving your session).

MassMine is now ready to work with Twitter. You can now start MassMine directly at any time, and you will never have to authenticate with Twitter again:

```sh
~/path/to/massmine
```
