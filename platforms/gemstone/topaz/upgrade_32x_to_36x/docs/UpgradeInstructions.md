# Upgrading from 3.2.15 to 3.6.2


## Overview
Upgrade is tested and supported from Rowan 1.2.10 on GemStone/S 64 bit v3.2.15 to Rowan 1.2.11 on GemStone/S 64 Bit v3.6.2.

The changes between these versions of GemStone require significant processing, and cannot be done in a single upgrade; an intermediate upgrade to v3.5.7 is required. Rowan 1.2.11 provides scripts to assist with the upgrade. These scripts need to be customized with the specific application details. 

There are many server changes between 3.2.x and 3.6.2. You should review the <i>Release Notes</i> for each intermediate version, most importantly for the major versions v3.3, v3.4, v3.5, and v3.6, to determine if your application source code or GemStone kernel class extension methods require changes. The Rowan upgrade process can load application source code with code modifications needed for 3.6.2. 

### Extension Methods on Kernel Classes
There have been many change in Kernel classes between 3.2.x and 3.6.x, which may affect your Kernel class extension method. Examine these carefully, to determine if these are still applicable or require modification.

#### Small* classes
In particular, the introduction of Small* magnitude classes in v3.6, which are "special" classes (the value is encoded in the OOP), introduces potential issues.

If application extension methods on GemStone kernel classes DateAndTime, Date, Time, or ScaledDecimal directly reference instance variables on that class, these will have issues when inherited by SmallDateAndTime, SmallDate, SmallTime, and SmallScaledDecimal, since specials by definition do not have instance variables. If you have extension methods on DateAndTime, Date, Time, or ScaledDecimal, you must review your code and ensure that any direct accesses to instance variables are changed to use accessor methods. Methods with direct instance variable references will compile correctly, but will get Error 2710/instVar access not allowed in a special object on execution. 


### Kernel Class Extension Method Packaging
Since the GemStone server upgrade removes all methods on all GemStone kernel classes prior to filing in the new methods, the upgrade process for Rowan-packaged kernel class extension methods requires special handling.

The upgrade process assumes that the application has a separate Project or Projects, that each contain only Packages containing kernel class method extensions. These Project/s should not contain application classes, nor extension methods on other application classes. If this is not the case, the <code>preUpgrade_v1.2.10.topaz</code> script will require further customization to avoid disconnecting application code. 


## Upgrade Instructions 
The following instructions provide details on upgrading a repository from Rowan 1.2.10 on GemStone/S 64 Bit 3.2.15, to Rowan 1.2.11 on GemStone/S 64 Bit v3.6.2

The upgrade from GemStone 3.2.15 to 3.6.2 requires: 

* Detaching from rowan in your existing repository; you must customize a pre-upgrade script to assist with this.
* the multi-step server upgrade to 3.5.7 then to 3.6.2
* Reloading Rowan and recompiling your application code, which requires customizing an application reload script.

The following instructions provide details on the Rowan upgrade specific steps, and a general overview for the server upgrade. If you are unfamiliar with the GemStone upgrade process, please review the Installation Guide for v3.5.x and for 3.6.x for detailed instructions on the server upgrade process. 

### 1.  Setup your environment
The 3.2.15 and 3.6.2 environments require $ROWAN_PROJECTS_HOME. For the server upgrade, the 3.5.7 and 3.6.2 environments require $GEMSTONE to be set to the appropriate directory. 

The Rowan login scripts, and GemStone server login scripts, require that the environment variable $upgradeLogDir is set to a writable directory. This should be set to the same directory for all steps of the upgrade. 
 <PRE CLASS="Code-Indented-Two">unix> export upgradeLogDir=/home/adminuser/upgradelogs</PRE>

### 2.  Reset the password for SystemUser
In your v3.2.15 repository, reset the password for SystemUser to 'swordfish'. The upgrades login as SystemUser, and reset the passwords for DataCurator and GcUser.

### 3.  Edit preUpgrade_v1.2.10.topaz 
You will first run a script to disconnect the Rowan projects and packages containing GemStone kernel class extension methods in your 3.2.15 repository. 

_Note that this script assumes that all kernel class extension methods are in packages in one or more projects, and that these projects do not contain any code other than kernel class extension methods._

The topaz file:
 <PRE CLASS="Code-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/upgrade_32x_to_36x/preUpgrade_v1.2.10.topaz</PRE>


performs this disconnect. There is no need to disconnect your other application projects or packages; only those that contain extension methods on kernel classes. Special handling is required since all kernel class methods are removed during the server upgrade process. 

Edit this file, replacing 'ExampleProject_GemStoneExtensions' 'ExampleProject_TestsGemStoneExtensions' with your specific project names. Be sure to include all projects that contain packages with kernel class extension methods. 

### 4.  Edit reload_application.topaz 
After the server upgrades and the Rowan code is loaded, the upgrade scripts will reload your application projects. 

The topaz file:
 <PRE CLASS="Code-Indented-Two">$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/upgrade_32x_to_36x/reload_application.topaz</PRE>


includes a list of the application projects to load.

Edit this file, replacing the lines of the form: 
 <PRE CLASS="Code-Indented-Two">'file:$ROWAN_PROJECTS_HOME/Example_Project_Main/rowan/specs/Example_Project_Main.ston'</PRE>


With the path to the load specifications for each of the application projects that were loaded in your 3.2.15 image and require reload.

This script should login as the GemStone userId that has loaded these projects, which may be SystemUser, DataCurator, or another user. Edit the login information if necessary.

### 5.  Execute preUpgradeRowan script in originating repository
The <b>preUpgradeRowan</b> script is executed on your 3.2.15 Rowan repository, and invokes the edited version of <code>preUpgrade_v1.2.10.topaz</code>. 

Make a backup of your application prior to executing this, since your 3.2.15 application will no longer be in a normally usable state.

This script takes one argument, the name of the running 3.2.15 Stone.
 <PRE CLASS="Code-Indented-Two">unix> $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/upgrade_32x_to_36x/preUpgradeRowan -s <i>stonename3215</i></PRE>


This should complete with the message:
 <PRE CLASS="Code-Indented-Two">preUpgradeRowan completed. No errors detected</PRE>


If not, examine the log files in $upgradeLogDir, and consult GemTalk Engineering.

After this step successfully completes, shut down the 3.2.15 stone.

### 6.  Upgrade the 3.2.15 image to v3.5.7
Upgrade is not supported directly from GemStone v3.2.15 to v3.6.2, so you must upgrade to an intermediate version. 

See Chapter 3 of the <A HREF="https://downloads.gemtalksystems.com/docs/GemStone64/3.5.x/GS64-InstallGuide-Linux-3.5.3.pdf" CLASS="URL">Installation Guide for v3.5.3 for Linux</A> for complete details. on upgrading from 3.2.15 to 3.5.7. You do not need to file out your kernel class modifications or application code, or recompile it; these steps are handled by the Rowan scripts.

The significant steps are:

  * Install and configure the GemStone 3.5.7 distribution, including a keyfile for 3.5.x. 
  * Ensure the 3.2.15 stone is shutdown, and copy the 3.2.15 extent files to the GemStone 3.5.7 location.
  * Execute <b>startstone</b> <i>stonename357</i>, using your normal <b>startstone</b> arguments.
  * Execute <b>upgradeImage -s</b> <i>stonename357</i>. You may wish to use the <b>-c</b><i> cacheSize</i> argument to improve performance on large repositories. 
  * You do not need to recompile methods; however, you should convert persistent SimpleBlocks in your application. Execute <b>postconv -s</b> <i>stonename</i>. 

If any of these steps reports error, please contact GemTalk Engineering. 

If <b>postconv</b> reports that it cannot convert some blocks, it will create a file <code>$upgradeLogDir/AllFailedSortedCollections.bm</code>. You will need to convert these blocks manually; this can be done after the Rowan upgrade is complete. 

Provided that the upgrade succeeded, the application is now upgraded to v3.5.7; but do not attempt to login or test, since application methods have not been recompiled. 

Shut down the stone. 

### 7.  Upgrade the 3.5.7 image to v3.6.2
Upgrade the repository from v3.5.7 to v3.6.2. Review the Installation Guide for v3.6 for details. 

The significant steps are:

  * Install and configure the GemStone 3.6.2 distribution, including a keyfile for 3.6.x. 
  * Ensure the 3.2.15 stone is shutdown, and copy the 3.5.7 extent files to the GemStone 3.6.2 location.
  * Execute <b>startstone</b> <i>stonename362</i>, using your normal startstone arguments.
  * Execute <b>upgradeImage -s</b> <i>stonename362</i>. You may wish to use the <b>-c</b><i> cacheSize</i> argument to improve performance on large repositories.

If any of these steps reports error, please contact GemTalk Engineering. 

Provided that the upgrade succeeded, the application is now upgraded to v3.6.2, but do not attempt to login or test yet, since application methods have not been recompiled. 

Leave the Stone running.

### 8.  Execute the upgradeRowan script
The <b>upgradeRowan</b> script is executed on your 3.6.2 repository, and invokes the edited version of <code>reload_application.topaz</code>. This script does the following:

  * installs the Rowan-related classes into your repository.
  * "adopt" these projects so they are functional in Rowan.
  * reload all the projects listed in <code>reload_application.gs</code>, ensuring that all methods are recompiled. The upgrade from 3.2.x to 3.6.x requires that all methods are recompiled, due to byte code changes in the GemStone server.

You must have edited <code>reload_application.gs</code>, to include your projects, before executing this script. 

If you have modified your application source code for compatibility with v3.6.2, be sure that the Rowan source code repository pointed to by $ROWAN_PROJECTS_HOME has the correct version for loading into v3.6.2.

Note that <code>reload_application.gs</code>, as written, expects to find your projects in SystemUser's symbolList. If you have loaded projects as a different user, edit the script's userId.

This script takes one argument, the name of the running 3.6.2 Stone.
 <PRE CLASS="Code-Indented-Two">unix> $ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/topaz/upgrade_32x_to_36x/preUpgradeRowan -s <i>stonename362</i></PRE>


This should complete with the message:
 <PRE CLASS="Code-Indented-Two">upgradeRowan completed. No errors detected</PRE>


If not, examine the log files in $upgradeLogDir, and consult GemTalk Engineering.

The application has now been upgraded and is ready to use.

### 9.  Restore passwords for system users
Change the password for SystemUser, DataCurator and GcUser, which have ben reset to 'swordfish' for the upgrade, back to the previous versions. 

### 10.  Make a backup
Make a backup of your system. 

Your application on Rowan 1.2.11 with GemStone/S v3.6.2 is now ready to use.

 




