# Rowan

Rowan is a new project/package manager for Smalltalk that supports FileTree and Tonel repositories.
 
## GemStone Installation

### GemStone 3.2.15 SystemUser
Installation instructions assume that you have registered SSH Keys with your GitHub account. See [Connecting to GitHub with SSH](https://help.github.com/articles/connecting-to-github-with-ssh/) for more information:

```
# Choose a standard location on disk where you will locate your GitHub project clones
#  and define the ROWAN_PROJECTS_HOME env var to reference this directory.
#  By default Rowan will clone GitHub projects into this directory. The directory may
#  be shared by multiple stones.
#
cd <GitHub clones directory>
export ROWAN_PROJECTS_HOME=`pwd`
git clone git@github.com:dalehenrich/Rowan.git

# (re)Start netldi (so that ROWAN_HOME is defined in gem environment for Jadeite)

# When updating, you do a "git pull origin master" before running the install. 
#  Alternately you may use the "Pull from Git" menu item in Jadeite

<start-topaz and setup stone params>

# script logs in as SystemUser and DataCurator
input $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/3.2.15/install.tpz
```

## Acknowledgements

Some portions of this reference implementation came from Pharo. Notably, the URL hierarchy of classes.
In addition, some scattered methods also came from Pharo.

Thanks to the Pharo project and community for providing such an excellent base.
