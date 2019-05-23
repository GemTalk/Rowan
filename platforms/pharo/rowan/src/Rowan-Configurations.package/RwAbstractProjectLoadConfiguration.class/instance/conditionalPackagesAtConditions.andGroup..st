accessing
conditionalPackagesAtConditions: conditions andGroup: groupName

	| thePackageNameMap |
	thePackageNameMap := (self conditionalPackages at: conditions asArray sort ifAbsent: [ ^ Set new])
		at: groupName ifAbsent: [ ^ Set new ].
	^ (thePackageNameMap at: #packageNames ifAbsent: [ Set new ]) asSet
