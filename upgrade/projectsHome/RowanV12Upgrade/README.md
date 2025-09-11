### Load script for RowanV12Upgrade project
Modify this project to update/change the way the RowanV12 project upgrade is handled.

There are 6 steps to the upgrade process:
1. install Rowan project
2. repair Rowan audit failures
3. repair customer audit failures
4. reload Rowan project
5. reload Customer project
6. final audit

The following script can be used to load the RowanV12Upgrade project into a Rowan stone and modify the Rowan upgrade process, if needed.
```
| specFilePath specUrlString spec repositoryRoot|
repositoryRoot := '$ROWAN_PROJECTS_HOME/RowanV1/upgrade/projectsHome/RowanV12Upgrade' asFileReference pathString.
specFilePath := repositoryRoot, '/rowan/specs/RowanV12Upgrade.ston'.
specUrlString := 'file:', specFilePath.
spec := RwSpecification fromUrl: specUrlString.
spec 
	repositoryRootPath: repositoryRoot;
	repositoryUrl: 'cypress:' , repositoryRoot , '/' , spec repoPath , '/'.
false ifTrue: [ self halt ].
spec register.
Rowan projectTools load loadProjectNamed: 'RowanV12Upgrade' withConfiguration: 'common'.
```

### exportUpgradeRowanV12Class.solo
Run the following script after making modifications to the RowanV12Upgrade project which contains
the upgrade logic for Rowan V1. The RowanV12Upgrade project is found in a Rowan V3 project in the
directory upgrade/projectsHome/RowanV12Upgrade. The Rowan V3 has the Rowan upgrade logic for Rowan V3, V2, and V1.

To run the .solo script you need to have $GEMSTONE defined pointing to 3.7.5 and $GEMSTONE/bin in your $PATH.

