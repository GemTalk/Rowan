# Upgrade Rowan and Application from 3.2.15 to 3.5.6 to 3.6.2
1. [3.2.15 Pre Upgrade Steps]()
2. [3.5.6 Upgrade Image]()
3. [3.6.2 Upgrade Image]()

## 1. 3.2.15 Pre Upgrade Steps: **v.1.2.10_preUpgrade.tpz**
In a 3.2.15 stone, execute the `$ROWAN_PROJECTS_HOME/platforms/gemstone/topaz/bootstrap/3.6.1/scripts/v.1.2.10_preUpgrade.tpz`. 

### Rowan project metadata removal
This script drops (disowns) the Rowan metadata for the Rowan projects: Rowan,
STON, Cypress and Tonel. The Rowan projects will be completely re-installed 
into the image after the 3.6.2 upgrade image step.

###Application project metadata removal for GemStone kernel class extension methods
This script also drops (disowns) the packages that contain extension methods
for the GemStone kernel classes. During upgrade image, all of the methods in
the GemStone kernel classes are removed, so in 3.6.2, the GemStone kernel 
extension methods will to be loaded from scratch. 

It is not necessary to remove the Rowan metadata for your Application classes.
The Rowan metadata for your Application will not be touched during upgradeImage.
All that will happen during the 3.6.2 Application upgrade step will be that
all methods in the Application classes will be recompiled.

## 2. 3.5.6 Upgrade Image
During this step you will run the GemStone upgrade image script, to 
upgrade the GemStone kernel classes and methods from 3.2.15 to GemStone 3.5.6.

There is no need perform any Rowan-specific or Application-specific processing
during this step.

## 3. 3.6.2 Upgrade Steps

### Upgrade Image
During this step you will run the GemStone upgrade image script, to 
upgrade the GemStone kernel classes and methods from GemStone 3.5.6 to GemStone 3.6.2.

### 3.6.2 Post Upgrade Image Steps: upgradeRowan.gs
In the freshly upgraded 3.6.2 stone, execute the `$ROWAN_PROJECTS_HOME/platforms/gemstone/topaz/bootstrap/upgradeRowan.gs`. 

#### install Rowan symbol dictionaries
This script ensures that the Rowan symbol dictionaries are present.

#### 3.6.1/RowanV2.gs
This script installs the Rowan v1.2.11 projects for GemStone 3.6.2.

#### adoptRowan.gs
This scripts creates the Rowan metadata (adopt) for the Rowan projects. 
At this point Rowan is fully functional.

#### reloadApplication.gs
First the `$ROWAN_PROJECTS_HOME/platforms/gemstone/topaz/bootstrap/3.6.1/scripts/reload_application.gs`
script will need to be editted to reflect that actual names and locations of the projects.

When executed the RwPrjUpgradeTool>>upgradeProjectFromSpecUrl: method will 
reload all of the application code into the 3.6.2 image for each of the listed projects.
It is a requirement that all methods that existed in the 3.2.15 image must be recompiled, 
whether or not the source of the method has changed.
The RwPrjUpgradeTool>>upgradeProjectFromSpecUrl: performs the same load operation as 
RwPrjLoadTool>>loadProjectFromSpecUrl: (recompile all methods with changed source, 
create new versions of classes, etc.), except that #upgradeProjectFromSpecUrl: ensures 
that all methods are unconditionally recompiled.

Any application code changes (method additions/changes/removal and class additions/changes/removal)that were made for 3.6.2 will be applied as well.

