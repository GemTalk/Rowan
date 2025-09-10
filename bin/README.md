### read_and_write_rowan_v.1.2.17_upgrade.solo
Run the following script after changing Rowan project code. The script generates the `platforms/gemstone/topaz/upgrade/3.7.5/RowanV12.gs` that contains the full Rowan V1 code base in a single .gs file, to be used as part of the Rowan V1 upgrade process or by the installRowan1.stone script to create a Rowan V1 stone:
```
$ROWAN_PROJECTS_HOME/RowanV1/bin/read_and_write_rowan_v.1.2.17_upgrade.solo \
		--projectsHome=/bosch1/users/dhenrich/_stones/37x/l_37x_externals_st \
		--projectAlias=RowanV1 --gemstoneVersion=3.7.5
```
To run the .solo script you need to have $GEMSTONE defined pointing to 3.7.5 and $GEMSTONE/bin in your $PATH.

### installRowan1.stone
Run the following script to install Rowan V1 into the stone named in the .topazini file. 
You should start the stone with a fresh extent0.dbf.
```
$ROWAN_PROJECTS_HOME/RowanV1/bin/installRowan1.stone \
		--rowanProjectsHome=$ROWAN_PROJECTS_HOME \
		--projectAlias=RowanV1 -- -L -I .topazini
```
To run the .solo script you need to have $GEMSTONE defined pointing to 3.7.5 and $GEMSTONE/bin in your $PATH.
NOTE that you should specify the path to the `.topazini` file if it is not in your current directory

### exportUpgradeRowanV12Class.solo
Run the following script after making modifications to the RowanV12Upgrade project which contains
the upgrade logic for Rowan V1. The RowanV12Upgrade project is found in a Rowan V3 project in the
directory upgrade/projectsHome/RowanV12Upgrade. The Rowan V3 has the Rowan upgrade logic for Rowan V3, V2, and V1.
```
$ROWAN_PROJECTS_HOME/RowanV1/bin/exportUpgradeRowanV12Class.solo \
    --projectsHome=$ROWAN_PROJECTS_HOME \ 
    --rowan3ProjectName=RowanV1 --rowan1ProjectName=RowanV1 -D
```


To run the .solo script you need to have $GEMSTONE defined pointing to 3.7.5 and $GEMSTONE/bin in your $PATH. 
NOTE that the exportUpgradeRowanV12Class.solo script will use $GEMSTONE/bin/extent0.rowan3.dbf to perform 
the necessary operations and why there is a rowan3 directory in upgrade/projectsHome/RowanV12Upgrade.

