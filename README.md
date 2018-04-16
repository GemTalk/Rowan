# Rowan

Rowan is a new project/package manager for Smalltalk that supports FileTree and Tonel repositories.
 
## GemStone Installation

### GemStone 3.2.15 SystemUser
```
export ROWAN_HOME=<path-to-Rowan-repository>

<start-topaz>

# script logs in as SystemUser and DataCurator
input $ROWAN_HOME/platforms/gemstone/topaz/3.2.15/install.tpz
```

### GsDevKit 3.2.15 SystemUser
After [installing GsDevKit_home](https://github.com/GsDevKit/GsDevKit_home#installation):
```
export rowan_stone_name=rowan_3215
createStone -f $rowan_stone_name 3.2.15

# clone Rowan

cd $GS_HOME/shared/repos
git clone git@github.com:dalehenrich/Rowan.git

GS_HOME/shared/repos/Rowan/platforms/gemstone/topaz/3.2.15/installRowan $rowan_stone_name
```

## Acknowledgements

Some portions of this reference implementation came from Pharo. Notably, the URL hierarchy of classes.
In addition, some scattered methods also came from Pharo.

Thanks to the Pharo project and community for providing such an excellent base.


