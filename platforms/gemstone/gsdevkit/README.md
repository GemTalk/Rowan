# [GsDevKit_home](https://github.com/GsDevKit/GsDevKit_home#open-source-development-kit-for-gemstones-64-bit-) support 

### GemStone/S 3.5.0 Installation (candidateV2.0 branch)
```
stoneName=rowan_350

cd $GS_HOME/shared/repos
export ROWAN_PROJECTS_HOME=`pwd`

git clone git@github.com:GemTalk/Rowan.git
git checkout candidateV2.0

createStone -G $stoneName 3.5.0

cd $GS_HOME/server/stones/$stoneName

ln -s $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/gsdevkit/stones/newBuild_rowan .
ln -s $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/gsdevkit/stones/newBuild_test_rowan .

cp $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/gsdevkit/stones/custom_stone.env .

stopNetldi $stoneName
startNetldi $stoneName

./newBuild_rowan

stones -r
```
In the `stones-r` output, not the `Port` used by `rowan_350_ldi` ... as that will be used for the Jadeite login.

### Jadeite Installation (Oscar-3.0.91)
At this point you should download and install [Jadeite Oscar-3.0.91](https://github.com/GemTalk/Jadeite/releases/tag/Oscar-3.0.91), following the [Jadeite installation instructions](https://github.com/GemTalk/Jadeite#runtime-installation).


