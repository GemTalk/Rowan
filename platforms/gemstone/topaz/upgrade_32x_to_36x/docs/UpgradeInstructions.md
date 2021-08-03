# Upgrading from 3.2.15 to 3.6.2
>

## Overview
Upgrade is tested and supported from Rowan 1.2.10 on GemStone/S 64 bit v3.2.15 or 3.2.17, to Rowan 1.2.11 on GemStone/S 64 Bit v3.6.2.

The changes between these versions of GemStone require significant processing, and cannot be done in a single upgrade; an intermediate upgrade to v3.5.7 is required. Rowan 1.2.11 provides scripts to assist with the upgrade. These scripts need to be customized with the specific application details. 

There are many server changes between 3.2.x and 3.6.2. You should review the <i>Release Notes</i> for each intermediate version, most importantly for the major versions v3.3, v3.4, v3.5, and v3.6. A full set of Release Notes for previous versions can be found at <A HREF="https://gemtalksystems.com/products/gs64/history/" CLASS="URL">https://gemtalksystems.com/products/gs64/history/</A>.

 

#### Extension Methods on Kernel Classes
There have been many change in Kernel classes between 3.2.x and 3.6.x. Examine your Kernel class method extensions carefully, and compare these with the 3.6.2 kernel class methods, to determine if these are applicable.

In particular, the introduction of Small* magnitude classes in v3.6, which are "special" classes (the value is encoded in the OOP), introduces potential issues.

If application extension methods on GemStone kernel classes DateAndTime, Date, Time, or ScaledDecimal directly reference instance variables on that class, these will have issues when inherited by SmallDateAndTime, SmallDate, SmallTime, and SmallScaledDecimal, since specials by definition do not have instance variables. If you have extension methods on DateAndTime, Date, Time, or ScaledDecimal, you must review your code and ensure that any direct accesses to instance variables are changed to use accessor methods. Methods with direct instance variable references will compile correctly, but will get Error 2710/instVar access not allowed in a special object on execution. 


## Upgrade Instructions 
The following instructions provide details on upgrading a repository from Rowan 1.2.10 on GemStone/S 64 Bit 3.2.15, to Rowan 1.2.11 on GemStone/S 64 Bit v3.6.2

The upgrade from GemStone 3.2.15 to 3.6.2 requires: 

   * Detaching from rowan in your existing repository; you must customize a pre-upgrade script to assist with this.
   * the multi-step server upgrade to 3.5.7 then to 3.6.2
   * Reloading Rowan and recompiling your application code, which requires customizing an application reload script.

The following instructions provide details on the Rowan upgrade specific steps, and a general overview for the server upgrade. If you are unfamiliar with the GemStone upgrade process, please review the Installation Guide for v3.5.x and for 3.6.x for detailed instructions on the server upgrade process. 

### 1.  Edit the v.1.2.10_preUpgrade.tpz script
You will first run a script to disconnect the Rowan repositories and kernel class extensions in your 3.2.15 repository. Make a backup of your application prior to executing this, since your 3.2.15 application will no longer be in a normally usable state. 

The script 
<PRE CLASS="Columns-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/<EM CLASS="NeedsReview">3.6.1</EM>/scripts/v.1.2.10_preUpgrade.tpz</PRE>


performs this disconnect of the Rowan repositories, and any packages contain kernel class extension methods. There is no need to disconnect your other application packages, only packages that contain methods on kernel classes, since the kernel methods will be removed during the upgrade process. 

Edit this script, replacing '...GemStoneExtensions' '...TestsGemStoneExtensions' with your specific package names. Be sure to include all packages that contain kernel class extension methods. 

Note that you will also need to edit this script to update the SystemUser password, and set your repository name and other login details needed for your v3.2.15 environment.

### 2.  Edit the reload_application script
After the server upgrades and the Rowan code is loaded, the upgrade scripts will reload your application packages. 

The script:
<PRE CLASS="Code-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/3.6.1/scripts/reload_application.gs</PRE>


includes a list of the application packages to load.

Edit this file, replacing the lines of the form: 
<PRE CLASS="Code-Indented-Two">'file:$ROWAN_PROJECTS_HOME/Example_Project_Missing/rowan/specs/Project_Missing.ston'</PRE>


With the path to the load specifications for each of the application projects that are loaded in your 3.2.15 image.

This script should login as the GemStone userId that has loaded these projects, which may be SystemUser, DataCurator, or another user. 

You may also need to edit this script for other login details.

### 3.  Execute v.1.2.10_preUpgrade.tpz script in originating repository
Start linked topaz in your v3.2.15 environment (do not login), and input the edited version of <code>v.1.2.10_preUpgrade.tpz</code>. 
<PRE CLASS="Code-Indented-Two">unix> topaz -l
<i>< startup details ></i>
topaz> input $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/<EM CLASS="NeedsReview">3.6.1</EM>/scripts/v.1.2.10_preUpgrade.tpz</PRE>

### 4.  Upgrade the 3.2.15 image to v3.5.7
Upgrade is not supported directly from GemStone v3.2.15 to v3.6.2, so you must upgrade to an intermediate version. 

Use the upgrade instructions in v3.5.7 to upgrade your repository from 3.2.15 to 3.5.7; see the Installation Guide for v3.5.3. You do not need to file out your kernel class modification or application code, or recompile it; these steps are handled by the Rowan scripts. 

The significant steps are:

   * Shut down the Stone and NetLDI, and reset the password for SystemUser to 'swordfish'.
   * Install and configure the GemStone 3.5.7 distribution.
   * Copy the 3.2.15 extent files to the GemStone 3.5.7 location.
   * Execute **startstone** <i>stonename</i>, using your normal **startstone** arguments.
   * Execute **upgradeImage -s** <i>stonename</i>. You may wish to use the **-c**<i> cacheSize</i> argument to improve performance on large repositories.
   * You do not need to recompile methods; however, you should convert persistent SimpleBlocks in your application. Execute **postconv -s** <i>stonename</i>. 

If any of these steps reports error, please contact GemTalk Engineering. 

If **postconv** reports that it cannot convert some blocks, it will create a file <code>$upgradeLogDir/AllFailedSortedCollections.bm</code>. You will need to convert these blocks manually; this can be done after the Rowan upgrade is complete. 

Provided that the upgrade succeeded, the application is now upgraded to v3.5.7, but do not attempt to login or test, since application methods have not been recompiled. 

Shut down the stone. 

### 5.  Upgrade the 3.5.7 image to v3.6.2
Now, upgrade the repository from v3.5.7 to v3.6.2. Review the Installation Guide for v3.6 for details. 

The significant steps are:

   * Install and configure the GemStone 3.6.2 distribution
   * Copy the 3.5.7 extent files to the GemStone 3.6.2 location
   * Execute **startstone** <i>stonename</i>, using your normal startstone arguments
   * Execute **upgradeImage -s** <i>stonename</i>. You may wish to use the **-c**<i> cacheSize</i> argument to improve performance on large repositories.

If any of these steps reports error, please contact GemTalk Engineering. 

Provided that the upgrade succeeded, the application is now upgraded to v3.6.2, but do not attempt to login or test yet, since application methods have not been recompiled. 

Leave the Stone running.

### 6.  	Execute the upgradeRowan.gs script
The script
<PRE CLASS="Code-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/upgradeRowan.gs</PRE>


performs the upgrade of Rowan and the recompile of your application code. You must have edited <code>reload_application.gs</code>, to include your projects, before executing this script. Note that by default, this expects to find your projects in SystemUser's symbolDictionary; if you have loaded projects as a different user, edit the script's userId. 

Start a linked topaz session (do not login), and input the script file:
<PRE CLASS="Code-Indented-Two">unix> topaz -l
<i>< startup details ></i>
topaz> input $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/bootstrap/upgradeRowan.gs</PRE>


This script does the following:

   * installs the Rowan-related classes into your repository.
   * "adopt" these projects so they are functional in Rowan
   * reload all the projects listed in <code>reload_application.gs</code>, ensuring that all methods are recompiled. The upgrade from 3.2.x to 3.6.x requires that all methods are recompiled, due to byte code changes in the GemStone server.

The application has now been upgraded and is ready to use.

### 7.  Restore passwords for system users
Change the password for SystemUser, DataCurator and GcUser, which have ben reset to 'swordfish' for the upgrade, back to the previous versions. 

### 8.  Make a backup
Make a backup of your system.

 

 
