#!/bin/bash

# Produces compressed directories containing binary distributions for massmine
if [ "$(uname)" == "Darwin" ]
then
 echo This script does not support Mac OS X platform yet
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
    chicken-install -deploy -p ./massmine clucker args openssl medea

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
