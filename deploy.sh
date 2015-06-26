#!/bin/bash

# Produces a directory "massmine" containing a binary distribution for massmine

# The executable and linked library
cd ~/main/development/massmine/
csc -deploy massmine.scm

# This step is currently required because of a bug in chicken/openssl
# that prevents openssl from being loaded in `eval`. This command
# copies CHICKEN's core libraries into the deployment directory. The
# bug is reported at https://bugs.call-cc.org/ticket/1191
chicken-install -i massmine

# Install clucker
cd ~/main/development/chicken/clucker/
chicken-install -deploy -p ~/main/development/massmine/massmine/
# Install additional chicken eggs not implicitly installed above.
cd ~/main/development/massmine/
chicken-install -deploy -p ./massmine args

# Package everything up with the current version number (zip and tarball)
zip massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.zip -r ./massmine/

tar czf massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.tar.gz ./massmine/

# Clean up by moving the results out of the development directory
mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.zip ~/deploy
mv massmine-`./massmine/massmine -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.tar.gz ~/deploy

# Remove the build directory
rm -rf massmine/
