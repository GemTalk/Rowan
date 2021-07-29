# Upgrading from 3.2.15 to 3.6.2

## Overview
Upgrade is testing and supported from Rowan 1.2.10 on GemStone/S 64 bit v3.2.15 and 3.2.17, to Rowan 1.2.11 on GemStone/S 64 Bit v3.6.2.   
The changes between these versions of GemStone require significant processesing, and cannot be done in a single upgrade; an intermediate upgrade to v3.5.7 is required.    
Rowan 1.2.11 provides scripts to assist with the upgrade. These scripts need to be customized with the specific application details.    
There are many server changes between 3.2.x and 3.6.2. You should review the <i>Release Notes</i> for each intermediate version, most importantly for v3.3, v3.4, v3.5, and v3.6. A full set of Release Notes for previous versions can be found at <A HREF="https://gemtalksystems.com/products/gs64/history/" CLASS="URL">https://gemtalksystems.com/products/gs64/history/</A>.   
 

### Extension Methods on Kernel Classes
There have been many change in Kernel classes between 3.2.x and 3.6.x. Examine your Kernel class method extensions carefully, and compare with the 3.6.2 kernel class methods, to determine if these are applicable.   
In particular, the introduction of SmallDateAndTime and other Small* magnitude classes, which are "special" classes, introduces some restrictions for methods on DateAndTime and superclasses of other Small* classes. If you have added a kernel class extension method to one of these classes, that accesses an instance variable directly (rather than using an accessor methods), this will break for the instances of the new subclasses. 

## Upgrade Instructions 
The following instructions provide details on upgrading a repository from Rowan 1.2.10 on GemStone/S 64 Bit 3.2.15, to Rowan 1.2.11 on GemStone/S 64 Bit v3.6.2   
Rowan 1.2.11 has been tested with and supports upgrade for applications running Rowan 1.2.10 on GemStone/S 64 Bit v3.2.15.   
The upgrade from GemStone 3.2.15 to 3.6.2 requires multiple step server upgrade, since server upgrade is not supported directly from 3.2.15 to 3.6.2. The upgrade also requires recompilation of all methods and sortblock. The scripts provided with Rowan 1.2.10 include the required additional processing to perform this upgrade.   
There are two scripts that you must customize with your own package and project names. These scripts also may need editing to provide the topaz login details for your application. 

##### Edit the v.1.2.10_preUpgrade.tpz script
You will first run a script to disconnect the Rowan repositories and kernel class extentsions in your 3.2.15 repository. Make a backup of your application prior to executing this, since your 3.2.15 application will no longer be in a normally usable state. The script <PRE CLASS="Columns-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/<EM CLASS="NeedsReview">3.6.1</EM>/scripts/v.1.2.10_preUpgrade.tpz</PRE>
<P CLASS="Spacer"></P>
performs this disconnect of the Rowan repositories, and any packages contain kernel class extension methods. There is no need to disconnect your other application packages, only packages that contain methods on kernel classes, since the kernel methods will be remoted during the upgrade process. Edit this script, replacing '...GemStoneExtensions' '...TestsGemStoneExtensions' with your specific package names. Be sure to include all packages that contain kernel class extension methods. Note that you will also need to edit this script to update the SystemUser password, and set your repository name and other login details needed for your v3.2.15 environment.

##### Edit the reload_application script
After the server upgrades and the Rowan code is loaded, the upgrade scripts will reload your application packages. The script:<PRE CLASS="Code-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/3.6.1/scripts/reload_application.gs</PRE>
<P CLASS="Spacer"></P>
includes a list of the application packages to load.Edit this file, replacing the lines of the form:'file:$ROWAN_PROJECTS_HOME/Example_Project_Missing/rowan/specs/Project_Missing.ston'With the path to the load specifications for each of the application projects that are loaded in your 3.2.15 image.This script should login as the GemStone userId that has loaded these projects, which may be SystemUser, DataCurator, or another user. You may also need to edit this script for other login details.    

##### Execute v.1.2.10_preUpgrade.tpz script in originating repository
Start linked topaz in your v3.2.15 environment. (do not login ), and input the edited version of <code>v.1.2.10_preUpgrade.tpz</code>.<PRE CLASS="Code-Indented-Two">topaz> input $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/<EM CLASS="NeedsReview">3.6.1</EM>/scripts/v.1.2.10_preUpgrade.tpz</PRE>

##### Upgrade the 3.2.15 image to v3.5.7
Upgrade is not supported directly from GemStone v3.2.15 to v3.6.2 , so you must upgrade to an intermediate version. Use the upgrade instructions in v3.5.7 to upgrade your repository from 3.2.15 to 3.5.7; see the Installation Guide for v3.5.3. You do not need to file out your kernel class modification or application code, or recompile it; these steps are handled by the Rowan scripts. The significant steps are:
* Shut down the Stone and NetLDI, and reset the password for SystemUser to 'swordfish'
* Install and configure the GemStone 3.5.7 distribution
* Copy the 3.2.15 extent files to the GemStone 3.5.7 location
* Execute **startstone**  <i>stonename</i>, using your normal **startstone**  arguments
* Execute **upgradeImage -s**  <i>stonename</i>. You may wish to use the **-c** <i>cacheSize</i> argument to improve performance on large repositories.
* Execute **postconv -s**  <i>stonename</i>, to convert stored simple SortBlocks in your application. 
If any of these steps reports error, please contact GemTalk Engineering. Provided that the upgrade succeeded, the application is now upgraded to v3.5.7, but do not attempt to login or test, since application methods have not been recompiled. Shut down the stone. 

##### Upgrade the 3.5.7 image to v3.6.2
Now, upgrade the repository from v3.5.7 to v3.6.2. Review the Installation Guide for v3.6 for details. The significant steps are:
* Install and configure the GemStone 3.6.2 distribution
* Copy the 3.5.7 extent files to the GemStone 3.6.2 location
* Execute **startstone**  <i>stonename</i>, using your normal startstone arguments
* Execute **upgradeImage -s**  <i>stonename</i>. You may wish to use the **-c** <i>cacheSize</i> argument to improve performance on large repositories.
If any of these steps reports error, please contact GemTalk Engineering. Provided that the upgrade succeeded, the application is now upgraded to v3.6.2, but do not attempt to login or test yet, since application methods have not been recompiled. Leave the Stone running.

##### 	execute upgradeRowan.gs script
The script<PRE CLASS="Code-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/upgradeRowan.gs</PRE>
<P CLASS="Spacer"></P>
performs the upgrade of Rowan and the recompile of your application code. You must have edited <code>reload_application.gs</code>, to include your projects, before executing this script. <PRE CLASS="Code-Indented-Two">topaz> input $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/upgradeRowan.gs</PRE>
<P CLASS="Spacer"></P>
This script does the following:
* installs the Rowan-related classes into your repository.
* "adopt" these projects so they are functional in Rowan
* reload all the projects listed in reload_application.gs, ensuring that all methods are recompiled. The upgrade from 3.2.x to 3.6.x requires that all methods are recompiled, due to byte code changes in the GemStone server.
The application has now been upgraded and is ready to use.

##### Change passwords for system users
Change the password for SystemUser, DataCurator and GcUser, which have ben reset to 'swordfish' for the upgrade, back to the previous versions. 

##### Make a backup
Make a backup of your system.    
    
  
