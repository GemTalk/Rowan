#! /bin/sh
set -xv
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

# make sure $GEMSTONE/bin in path for .solo and .stone scripts
PATH=$GEMSTONE/bin:$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin:$PATH; export PATH

#=========================================================================
#	wire up variables for testing
#=========================================================================
stoneName=battery_l
upgradeFrom=3.6.2

cd /bosch1/users/dhenrich/_stones/37x/stones/$stoneName

#======
# set up upgrade directories
#======
export upgradeLogDir=`pwd`/upgradeLogDir
rm -rf upgradeLogDir
mkdir upgradeLogDir
export upgradeImageLogPath=$upgradeLogDir/upgradeImage.log
export upgradeFir=$GEMSTONE/upgrade

# Start Stone
newExtent.solo --registry=37x $stoneName --extent=/export/smalltalk/rowanupgradetest/rowanV12/$upgradeFrom/extent0_RowanV1.2.14.dbf

# Run upgradeImage
pushd $upgradeLogDir
	$GEMSTONE/bin/upgradeImage -s $stoneName > $upgradeImageLogPath << EOF

EOF
popd

# Run RowanV12 upgrade
$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin/upgradeImageRowanV12.stone --upgradeFrom=$upgradeFrom --customerRepair --commit --installRowan --rowanRepair --rowanReload --rowanVersion=candidateV1.2.17  --debugGem -- -L  -I .topazini -e ./gem.conf

