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
rowanV1Upgrade.sh [-b <<branch-name>] [-C  <customer-topazini-path>] [-d ] \
    [-S <SystemUser-topazini-path>] [-s <stoneName>] [-u] \
    [-v <original-gemstone-version>][-s <stoneName>] [-e <gem-conf-file> ]

NOTE:
  If the customer projects are installed as SystemUser, then the -S
  option will be sufficient to upgrade the customer projects.

  If the customer projects are installed as another GemStone user, then
  use the -C option to specify the user name to be used to upgrade
  the customer projects for that user. The -C option can be used more
  than once in separate script runs, if customer projects are installed
  in multiple users.

Environment Requirements:
    GEMSTONE          set to a 3.x GemStone/S 64 Bit product tree
    upgradeLogDir     set to a writable directory used in previous steps
Parameters:
    -b <branch-name>
        Name of the Rowan V1 git branch to be loaded during the upgrade. 
        Default: master
    -C <customer-topazini-path>
        Path to the customer project .topazini file. Necessary if the
        customer project is not installed as SystemUser.
    -d
        debug the upgradeImageRowanV12.stone script ... bring up topaz
        debugger in case of an execution error.
    -e <gem-conf-file>
        path to gem.conf file if desired
		-S <SystemUser-topazini-path>
        Path to the SystemUser topazini file path.
    -s <stoneName>
        where <stoneName> is the name of a running 3.x stone.
        Default: gs64stone
    -u
       run the standard upgradeImage on the stone BEFORE upgrading Rowan 
       and customer projects
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

runUpgradeImage="false"

COMBINED_RUN="false"
SystemUser_RUN="false"
Customer_RUN="false"

defaultTopazini="./.topazini"

customerTopazini=""
systemTopazini=""

gem_conf_file_option=""

# process command line
while getopts "b:C:dS:s:uv:" opt; do
  case $opt in 
  	b ) expectedBranchName=$OPTARG ;;
		C ) customerTopazini="$OPTARG" ;;
    d ) debugGem="-D" ;;
		e ) gem_conf_file_option="-e $OPTARG" ;;
		S ) systemTopazini="$OPTARG" ;;
    s ) stoneName=$OPTARG ;;
		u ) runUpgradeImage="true" ;;
    v ) upgradeFrom="$OPTARG" ;;
   \? ) usage; exit 1 ;;   
  esac
done

if [ "a$customerTopazini" = "a" ]; then
	if [ "a$systemTopazini" = "a" ]; then
		customerTopazini=$defaultTopazini
		systemTopazini=$defaultTopazini
		# customer projects are installed as SystemUser
		COMBINED_RUN="true"
	else
		#customer projects are installed as SystemUser
		customerTopazini=$systemTopazini
		COMBINED_RUN="true"
	fi
else
	if [ "a$systemTopazini" = "a" ]; then
		# customer only run
		SystemUser_RUN="false"
		Customer_RUN="true"
	else
		# customer and system run ... each using a different .topazini
		SystemUser_RUN="true"
		Customer_RUN="true"
	fi
fi

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
		currentBranch=`git rev-parse --abbrev-ref HEAD`
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

if [ "$runUpgradeImage" = "true" ]; then
	# Start Stone
	newExtent.solo --registry=37x $stoneName --extent=/bosch1/users/dhenrich/_stones/37x/stones/plantis_3.7.5_l/snapshots/extent0_plantis_362.dbf $debugGem

	echo "Run upgradeImage"
	$GEMSTONE/bin/upgradeImage -s $stoneName > $upgradeImageLogPath << EOF

EOF
else
	# verify that the extent has been upgraded to 3.7.5 ... `obj DBFHistory` dumps an informative mesage to stdout
	$GEMSTONE/bin/topaz -i -l $gem_conf_file_option  -I $systemTopazini << EOF > $upgradeLogDir/rowanUpgradeCheck.out

set gemstone $stoneName
set user SystemUser pass swordfish
status
login

obj DbfHistory

expectValue true
run
| gsVers |
gsVers := System gemVersionReport at: 'gsVersion'.
gsVers = '3.7.5'
GsFile stdout
	nextPutAll: 'Current GemStone version is ', gsVers;
	lf.
gsVers = '3.7.5'
%
EOF

	topaz_stat=$?
	if [ $topaz_stat -eq 0 ]; then
		echo "Confirmed that the extent has been upgraded to 3.7.5"
	else
		echo "It appears that GemStone has not been upgraded to 3.7.5. Please run upgradeImage on your stone before running this script. Check $upgradeLogDir/rowanUpgradeCheck.out for details "
		exit $topaz_stat
	fi
fi

if [ "$COMBINED_RUN" = "true" ]; then
	# Run RowanV12 upgrade where customer project installed as SystemUser
	echo "combined run using _ $systemTopazini _"
	$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin/upgradeImageRowanV12.stone --upgradeFrom=$upgradeFrom --customerRepair --customerReload \
		--commit --installRowan --rowanRepair --rowanReload $debugGem -- -L  -I $systemTopazini $gem_conf_file_option 
else
	# Run RowanV12 upgrade where customer project installed as an alternate user
	if [ "$SystemUser_RUN" = "true" ]; then
		echo "SystemUser run using _ $systemTopazini _ "
		$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin/upgradeImageRowanV12.stone --upgradeFrom=$upgradeFrom \
			--commit --installRowan --rowanRepair --rowanReload $debugGem -- -L  -I $systemTopazini $gem_conf_file_option
	fi
	if [ "$Customer_RUN" = "true" ]; then
		echo "Customer run using _ $customerTopazini _ "
		$ROWAN_PROJECTS_HOME/Rowan/upgrade/bin/upgradeImageRowanV12.stone --upgradeFrom=$upgradeFrom --customerRepair --customerReload \
			--commit $debugGem -- -L  -I $customerTopazini $gem_conf_file_option
	fi
fi
echo "### Rowan V1 upgrade complete"
