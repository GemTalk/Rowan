# Rowan

Rowan is a new project/package manager for Smalltalk that supports FileTree and Tonel repositories.
 
## GemStone Installation

### GsDevKit_home Rowan installation

```
# GsDevKit_home installation

git clone https://github.com/GsDevKit/GsDevKit_home.git
cd GsDevKit_home
. bin/defHOME_PATH.env    # define GS_HOME env var and put $GS_HOME into PATH
installServerClient

# create tode client
createClient tode

# create a Rowan stone
createStone -f test_rowan_340 3.4.0

# clone Rowan

cd shared/repos
git clone git@github.com:dalehenrich/Rowan.git

# setup GsDevKit_home for Rowan stones

$GS_HOME/shared/repos/Rowan/src/platforms/gemstone/gsdevkit/bin/setupRowanGsDevKit

# install Rowan into stone
$GS_HOME/shared/repos/Rowan/src/platforms/gemstone/gsdevkit/bin/installRowan test_rowan_340

```

## Acknowledgements

Some portions of this reference implementation came from Pharo. Notably, the URL hierarchy of classes.
In addition, some scattered methods also came from Pharo.

Thanks to the Pharo project and community for providing such an excellent base.

