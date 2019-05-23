accessing
conditionalPackagesAtConditions: conditions andGroup: groupName addPackageNames: packageNames

	| thePackageNames thePackageNameMap |
	thePackageNameMap := (self conditionalPackages at: conditions asArray sort ifAbsentPut: [ Dictionary new])
		at: groupName ifAbsentPut: [ Dictionary new ].
	thePackageNames := (thePackageNameMap at: #packageNames ifAbsentPut: [ Set new ]) asSet.
	thePackageNames addAll: packageNames.
	thePackageNameMap at: #packageNames put: thePackageNames asArray sort
