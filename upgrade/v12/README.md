## After making changes to RowanV1 tone files

### Generate the RowanV1/platforms/gemstone/topaz/upgrade/3.7.5/RowanV12.gs file
The RowanV12.gs file is an export of the RowanV1 classes and methods. 
The RowanV12.gs is used to bootstrap RowanV1 into a 3.7.5 stone and as part of the upgrade process 
for upgrading to a RowanV1 extent to 3.7.5
```
export PATH=$GEMSTONE/bin:$PATH
export RowanV1ProjectsHome=/bosch1/users/dhenrich/_stones/37x/stones/gs_375/v12
$RowanV1ProjectsHome/RowanV1/platforms/gemstone/topaz/upgrade/read_and_write_rowan_v.1.2.17_upgrade.solo \
     --projectsHome=$RowanV1ProjectsHome --projectAlias=Rowan --gemstoneVersion=3.7.5
```

### Generate the RowanV3/upgrade/v12/UpgradeRowanV12
After making changes to the RowanV3/upgrade/projectsHome/RowanV12Upgrade project using RowanV3 to make 
the changes, you need to create .gs file that installs the RowanV1 upgrade support classes.
```
export RowanProjectsHome=/bosch1/users/dhenrich/_stones/37x/l_37x_externals_st
export RowanV1ProjectsHome=/bosch1/users/dhenrich/_stones/37x/stones/gs_375/v12
export PATH=$GEMSTONE/bin:$PATH

$RowanProjectsHome/RowanV3/upgrade/v12/bin/exportUpgradeRowanV12Class.solo \
     --projectsHome=$RowanV1ProjectsHome --projectName=RowanV1
```
### Bootstrap RowanV1 into a 3.7.5 extent
Create and start a 3.7.5 stone.
Run the bin/installRowanV1.topaz script in the stone:
```
export RowanProjectsHome=/bosch1/users/dhenrich/_stones/37x/stones/gs_375/v12

# using topaz directly to input the file $RowanProjectsHome/bin/installRowanV1.topaz
# OR use the superdoit .topaz script directly
export PATH=$GEMSTONE/bin:$PATH
$RowanProjectsHome/bin/installRowanV1.topaz




```

### Upgrade a RowanV1 extent to 3.7.5
