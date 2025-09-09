#! /bin/bash
set -exv
#=========================================================================
# Copyright (C) GemTalk Systems 1986-2025.  All Rights Reserved.
#
# Name - rowanV1Upgrade.sh
#
# Purpose - 
#
# Requirements:
# 
# The following environment variables should be set: 
#	GEMSTONE 	- set to the GemStone/S 64 product tree.
#       upgradeLogDir   - a writable scratch directory.
#       GEMSTONE_SYS_CONF - 
#
# Rowan V1 CUSTOMER UPGRADE PROCEDURE
#   
#
#=========================================================================

#=========================================================================
#	argument handling
#=========================================================================
usage() {
  cat <<EOF
Usage:
rowanV1Upgrade.sh [-v <original-gemstone-version>][-s <stoneName>]
Environment Requirements:
    GEMSTONE          set to a 3.x GemStone/S 64 Bit product tree
    upgradeLogDir     set to a writable directory used in previous steps
Parameters:
    -b <branch-name>
        Name of the Rowan V1 git branch to be loaded during the upgrade. 
        Default: master
    -d
        debug the upgradeImageRowanV12.stone script ... bring up topaz
        debugger in case of an execution error.
    -s <stoneName>
        where <stoneName> is the name of a running 3.x stone.
        Default: gs64stone
    -v <original-gemstone-version>]
        original GemSTone version that was used to produce extent0.dbf 
        being upgraded.
        Default: 3.6.2
EOF
}

stoneName=gs64stone
upgradeFrom="3.6.2"
expectedBranchName="master"
debugGem=""

# process command line
while getopts "b:ds:v:" opt; do
  case $opt in 
  	b ) expectedBranchName=$OPTARG ;;
    d ) debugGem="-D" ;;
    s ) stoneName=$OPTARG ;;
    v ) upgradeFrom="$OPTARG" ;;
   \? ) usage; exit 1 ;;   
  esac
done

if [ "a$GEMSTONE" = "a" ]; then
  echo "ERROR: GemStone scripts require a GEMSTONE environment variable."
  echo "       Please set it to the directory where GemStone resides."
  exit 1
fi
if [ "a$ROWAN_PROJECTS_HOME" = "a" ]; then
  echo "ERROR: Rowan v1 upgrade scripts require a ROWAN_PROJECTS_HOME environment variable,"
  echo "       defining the parent directory of the Rowan v1 git clone."
  exit 1
fi

# make sure that Rowan V1 is present in $ROWAN_PROJECTS_HOME and that the correct branch is checkout out.
#		clone if not present

if [ -d "$ROWAN_PROJECTS_HOME/Rowan" ]; then
	echo "Rowan project is already present in $ROWAN_PROJECTS_HOME"
	pushd $ROWAN_PROJECTS_HOME/Rowan
		currentBranch=`git branch --show-current`
		if [ "$currentBranch" != "$expectedBranchName" ]; then
			echo "incorrect Rowan v1 branch is currently checked out: $currentBranch. Expected candidateV1.2.17"
			exit 1
		else
			echo "Rowan v1 branch is $currentBranch"
		fi
	popd
else
	echo "cloning Rowan V1 project into $ROWAN_PROJECTS_HOME"
	pushd $ROWAN_PROJECTS_HOME
		git clone -b candidateV1.2.17 -- git@github.com:GemTalk/Rowan.git Rowan
	popd
fi
# make sure $GEMSTONE/bin in path for .solo and .stone scripts
PATH=$GEMSTONE/bin:$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin:$PATH; export PATH

#======
# set up upgrade directories
#======
export upgradeLogDir=`pwd`/upgradeLogDir
rm -rf upgradeLogDir
mkdir upgradeLogDir
export upgradeImageLogPath=$upgradeLogDir/upgradeImage.log
export upgradeFir=$GEMSTONE/upgrade

# Start Stone
newExtent.solo --registry=37x $stoneName --extent=/export/smalltalk/rowanupgradetest/rowanV12/$upgradeFrom/extent0_RowanV1.2.14.dbf $debugGem

# Run upgradeImage
$GEMSTONE/bin/upgradeImage -s $stoneName > $upgradeImageLogPath << EOF

EOF

# Run RowanV12 upgrade
$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin/upgradeImageRowanV12.stone --upgradeFrom=$upgradeFrom --customerRepair --customerReload --commit --installRowan --rowanRepair --rowanReload $debugGem -- -L  -I .topazini -e ./gem.conf

echo "### Rowan V1 upgrade complete"
