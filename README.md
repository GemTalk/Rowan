# Rowan

Rowan is a new project/package manager for Smalltalk that supports FileTree and Tonel repositories.
 
## GemStone Installation

### GemStone 3.2.15 SystemUser
```
cd <working area>
git clone git@github.com:dalehenrich/Rowan.git
cd Rowan
export ROWAN_HOME=`pwd`

# (re)Start netldi (so that ROWAN_HOME is defined in gem environment for Jadeite)

<start-topaz and setup stone params>

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

cat -- >> $GS_HOME/server/stones/$rowan_stone_name/custome_stone.env << EOF
export ROWAN_HOME=$GS_HOME/shared/repos/Rowan
EOF
stopNetldi $rowan_stone_name
startNetldi $rowan_stone_name

$GS_HOME/shared/repos/Rowan/platforms/gemstone/topaz/3.2.15/installRowan $rowan_stone_name
```

## Acknowledgements

Some portions of this reference implementation came from Pharo. Notably, the URL hierarchy of classes.
In addition, some scattered methods also came from Pharo.

Thanks to the Pharo project and community for providing such an excellent base.


