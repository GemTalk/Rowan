# Rowan

Rowan is a new project/package manager for Smalltalk that supports FileTree and Tonel repositories.
 
## GemStone Installation
### GemStone 3.2.15 

#### Prerequisites
Installation instructions assume that you have registered SSH Keys with your GitHub account. See [Connecting to GitHub with SSH](https://help.github.com/articles/connecting-to-github-with-ssh/) for more information.

You must have git installed: [git setup](https://help.github.com/articles/set-up-git/)

You must have GemStone installed on this node, and a GemStone repository setup and running. For GemStone installation, see [GemStone 3.2.x Installation instructions for Linux](https://downloads.gemtalksystems.com/docs/GemStone64/3.2.x/GS64-InstallGuide-Linux-3.2.6/GS64-InstallGuide-Linux-3.2.6.htm). 

If you already have GemStone running, after defining the ROWAN_PROJECTS_HOME environment variable, you will need to restart the NetLDI.

#### Installation

Choose a standard location on disk where you will locate your GitHub project clones and define the ROWAN_PROJECTS_HOME env var to reference this directory.
By default Rowan will clone GitHub projects into this directory. The directory may be shared by multiple stones.

```
cd <GitHub clones directory>
export ROWAN_PROJECTS_HOME=`pwd`
git clone git@github.com:GemTalk/Rowan.git
```

If you have already performed the clone, and are re-installing Rowan in a new GemStone extent, do a "git pull origin master" before running the install. Alternately you may use the "Pull from Git" menu item in Jadeite.

Start topaz, and enter the GemStone parameters for login. If you are new to GemStone, see the [Topaz User's Guide](https://downloads.gemtalksystems.com/docs/GemStone64/3.3.x/GS64-Topaz-3.3/1-Tutorial.htm#pgfId-1069219)

The script logs in as SystemUser and DataCurator.  Ensure that the passwords for these users are set to the default.

```
input $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/3.2.15/install.tpz
```



## Acknowledgements

Some portions of this reference implementation came from Pharo. Notably, the URL hierarchy of classes.
In addition, some scattered methods also came from Pharo.

Thanks to the Pharo project and community for providing such an excellent base.

