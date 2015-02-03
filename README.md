# MassMine

*WARNING: This project is currently in pre-release form. Day to day changes are likely to break existing functionality. Upcoming version 1.0.0 will mark the first stable release*

![Splash Screen](https://github.com/n3mo/massmine/raw/master/img/splash.png)

## Installation and Usage
Documentation and installation instructions are available at [www.massmine.org](http://www.massmine.org)

## About
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

## License

Copyright (C) 2014-2015 Nicholas M. Van Horn & Aaron Beveridge

Author: Nicholas M. Van Horn <nvanhorn@nicholasvanhorn.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
