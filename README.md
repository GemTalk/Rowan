# Rowan

Rowan is a new project/package manager for Smalltalk that supports FileTree and Tonel repositories.
 
## GemStone Installation

### GsDevKit_home Rowan installation for GemStone3.4.x

```
# GsDevKit_home installation

git clone https://github.com/GsDevKit/GsDevKit_home.git
cd GsDevKit_home
. bin/defHOME_PATH.env    # define GS_HOME env var and put $GS_HOME into PATH
installServerClient

# use dev branch of tode
cd $GS_HOME/shared/repos/tode
git checkout dev
git pull origin dev

# create a Rowan stone
export rowan_stone_name=test_rowan_340
createStone -f $rowan_stone_name 3.4.0

# clone Rowan

cd $GS_HOME/shared/repos
git clone git@github.com:dalehenrich/Rowan.git

# setup GsDevKit_home for Rowan stones

$GS_HOME/shared/repos/Rowan/platforms/gemstone/gsdevkit/bin/setupRowanGsDevKit

# install Rowan into stone
$GS_HOME/shared/repos/Rowan/platforms/gemstone/gsdevkit/bin/installRowan $rowan_stone_name 340

```

### GsDevKit_home Rowan installation for GemStone3.5.0-EA-43770
```
# GsDevKit_home installation

git clone https://github.com/GsDevKit/GsDevKit_home.git
cd GsDevKit_home
. bin/defHOME_PATH.env    # define GS_HOME env var and put $GS_HOME into PATH
installServerClient

# use dev branch of tode
cd $GS_HOME/shared/repos/tode
git checkout dev
git pull origin dev

# download GemStone3.5.0-EA-43770
downloadGemStone -f -d GemStone3.5.0-EA-43770

# create a Rowan stone
export rowan_stone_name=test_rowan_350
createStone -f $rowan_stone_name 3.5.0

# clone Rowan

cd $GS_HOME/shared/repos
git clone git@github.com:dalehenrich/Rowan.git

# setup GsDevKit_home for Rowan stones

$GS_HOME/shared/repos/Rowan/platforms/gemstone/gsdevkit/bin/setupRowanGsDevKit

# install Rowan into stone
$GS_HOME/shared/repos/Rowan/platforms/gemstone/gsdevkit/bin/installRowan $rowan_stone_name 350

```


## Rowan Package Smalltalk scripts
```Smalltalk
"git pull for Rowan project"
Rowan projectTools pull pullSpecUrl: 'Rowan'.

"write listed packages and commit Rowan project"
Rowan packageTools commit
	commitSpecUrl: 'Rowan'
	packageNames: #('Rowan-Services')
	message: 'example commit message'.

"git push for Rowan project"
Rowan projectTools push pushSpecUrl: 'Rowan'.

```

## Rowan Project Smalltalk scripts
```Smalltalk
"git pull for Rowan project"
Rowan projectTools pull pullSpecUrl: 'Rowan'.

"write changed packages and commit Rowan project"
Rowan projectTools commit
	commitSpecUrl: 'Rowan'
	message: 'example commit message'.

"git push for Rowan project"
Rowan projectTools push pushSpecUrl: 'Rowan'.

"in-image changes"
(Rowan projectTools diff diffSpecUrl: 'Rowan') asString.
```


## Acknowledgements

Some portions of this reference implementation came from Pharo. Notably, the URL hierarchy of classes.
In addition, some scattered methods also came from Pharo.

Thanks to the Pharo project and community for providing such an excellent base.

