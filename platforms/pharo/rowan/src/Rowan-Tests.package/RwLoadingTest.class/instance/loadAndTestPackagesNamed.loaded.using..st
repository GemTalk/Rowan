support
loadAndTestPackagesNamed: packageNames loaded: packagesShouldBeLoaded using: myPackageSet

	| imagePackageSet1 diff1 imagePackageSet2 diff2 |
	packagesShouldBeLoaded
		ifFalse: [ packageNames do: [ :name | self assertAbsenceOfPackage: name ] ].

	imagePackageSet1 := self _loadedPackageDefinitionsNamed: packageNames.
	diff1 := myPackageSet compareAgainstBase: imagePackageSet1.
	self deny: diff1 isEmpty.

	[ Rowan packageTools load loadPackageSetDefinition: myPackageSet ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	imagePackageSet2 := self _loadedPackageDefinitionsNamed: packageNames.
	diff2 := myPackageSet compareAgainstBase: imagePackageSet2.
	self assert: diff2 isEmpty
