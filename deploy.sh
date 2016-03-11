#!/bin/bash

# Produces compressed directories containing binary distributions for massmine

# The linux deployment script should work on any properly set-up Linux
# computer when ran from inside the root massmine/ directory. The Mac
# OS X script won't work on other user's computers

if [ "$(uname)" == "Darwin" ]
then
    # The executable and linked library
    csc -deploy massmine.scm

    # This step is currently required because of a bug in chicken/openssl
    # that prevents openssl from being loaded in `eval`. This command
    # copies CHICKEN's core libraries into the deployment directory. The
    # bug is reported at https://bugs.call-cc.org/ticket/1191
    
    # To solve the above problem, I have built a local version of the
    # http-client egg that simply adds openssl to the use statement at
    # the beginning of the egg. Installing it manually from its
    # directory (using `sudo chicken-install` from its directory)
    # makes everything work as expected on OS X
    cd ../chicken/http-client/
    sudo chicken-install -deploy -p ../../massmine/massmine
    cd ../../massmine/
    
    # Install clucker and additional eggs
    sudo chicken-install -deploy -p ./massmine clucker args medea openssl srfi-19 pathname-expand html-parser

    # Package everything up with the current version number (zip and tarball)
    zip massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.zip -r ./massmine/

    tar czf massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.tar.gz ./massmine/

    # Clean up by moving the results out of the development directory
    mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.zip ~/deploy
    mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.tar.gz ~/deploy

    # Remove the build directory
    rm -rf massmine/

elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]
then
    # Do something under Linux platform
    # The executable and linked library
    csc -deploy massmine.scm

    # This step is currently required because of a bug in chicken/openssl
    # that prevents openssl from being loaded in `eval`. This command
    # copies CHICKEN's core libraries into the deployment directory. The
    # bug is reported at https://bugs.call-cc.org/ticket/1191
    chicken-install -i massmine

    # Install clucker and additional eggs
    chicken-install -deploy -p ./massmine clucker args openssl medea srfi-19 pathname-expand html-parser

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
