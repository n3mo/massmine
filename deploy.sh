#!/bin/bash

# Produces compressed directories containing binary distributions for massmine

# The linux deployment script should work on any properly set-up Linux
# computer when ran from inside the root massmine/ directory. The Mac
# OS X script won't work on other user's computers

if [ "$(uname)" == "Darwin" ]
then

    # Note that building on MacOS requires some care involving
    # openssl, given that Mac no longer supports it. We must let csc
    # know where to find things (installed via homebrew)
    export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
    export CPPFLAGS="-I/usr/local/opt/openssl@3/include"
    export PKG_CONFIG_PATH="/usr/local/opt/openssl@3/lib/pkgconfig"
    export CSC_OPTIONS="-I/usr/local/opt/openssl@3/include -L/usr/local/opt/openssl@3/lib"

    # Create build directory
    mkdir ./massmine

    # Do something under Linux platform
    # The executable and linked library
    csc -static massmine.scm -L -lssl -L -lcrypto -o ./massmine/massmine

    # Package everything up with the current version number (zip and tarball)
    zip massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-11.zip -r ./massmine/

    tar czf massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-11.tar.gz ./massmine/

    # Clean up by moving the results out of the development directory
    mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-11.zip ~/deploy
    mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-11.tar.gz ~/deploy

    # Remove the build directory
    rm -rf massmine/


elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]
then

    # Create build directory
    mkdir ./massmine

    # Do something under Linux platform
    # The executable and linked library
    csc -static massmine.scm -L -lssl -L -lcrypto -o ./massmine/massmine

    # Package everything up with the current version number (zip and tarball)
    zip massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.zip -r ./massmine/

    tar czf massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.tar.gz ./massmine/

    # Clean up by moving the results out of the development directory
    mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.zip ~/deploy
    mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.tar.gz ~/deploy

    # Remove the build directory
    rm -rf massmine/

elif [ -n "$COMSPEC" -a -x "$COMSPEC" ]
then 
  echo $0: this script does not support Windows \:\(
fi

# end of file deploy.sh
