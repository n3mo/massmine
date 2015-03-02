# json2csv
Convert (and subset) json files to csv from the command line

`json2csv` converts a json stream (from file) to a comma separated values (csv) file. Conversion is fast and does not consume memory, allowing for conversion of arbitrarily large json files. Data fields can optionally be filtered during conversion so that only desired fields are retained in the resulting csv file. For example, by keeping only the most relevant data fields, `json2csv` reduced a 5.6GB file of Twitter data in json format to a 158MB csv file.

## Usage

Assuming conversion of a file called `mydata.json`, the following commands will create a csv file called `mydata.csv`.

**To convert the file mydata.json to mydata.csv:**

    json2csv mydata.json

**To convert AND only keep the data fields "text" and "created_on":**

    json2csv mydata.json text created_on

**To remove data fields, preface field names with a "-". The following will keep all fields EXCEPT "user" and "address":**

    json2csv mydata.json -user -address

## Installation

### Dependencies

### R

json2csv requires that R is installed on your computer. R is available for all major operating systems, and installs easily. Installation instructions can be found at [The R Project for Statistical Computing](http://www.r-project.org/).

On Arch Linux, you can install R from the command line:

```sh
sudo pacman -S r
```

For Ubuntu (and similar OSes), you can install R from the command line:

```sh
sudo apt-get install r-base
```

### Install json2csv

The easiest way to install json2csv is with git. From the command line:

```sh
git clone https://github.com/n3mo/json2csv.git
chmod +x ~/path/to/json2csv
```

### R packages
json2csv requires the following R packages. To install, you can simply run json2csv. json2csv will return a shell command that you can manually run to install all missing packages. Alternatively, you can install these dependencies yourself by starting an interactive R session with `R` from the command line. From within R, you install *package-name* with:

```R
install.packages("package-name")
```

**Dependencies (R Packages)**
- tools (installed by default with most R binaries)
- plyr
- jsonlite

## License

Copyright (C) 2015 Nicholas M. Van Horn

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
