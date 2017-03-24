"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: subtests
"
loadInitialPackages

	| packageNames myPackageSet imagePackageSet1 diff1 imagePackageSet2 diff2 |
	packageNames := #('TestPackage1' 'TestPackage2').
	packageNames do: [:name | self assertAbsenceOfPackage: name].
	imagePackageSet1 := self definitionsOfLoadedPackages: packageNames.
	myPackageSet := CypPackageSetDefinition new.
	myPackageSet
		addPackage: self package1Definition;
		addPackage: self package2Definition.
	diff1 := myPackageSet againstBase: imagePackageSet1.
	self deny: diff1 isEmpty.
	self applyDiffToImage: diff1.
	imagePackageSet2 := self definitionsOfLoadedPackages: packageNames.
	diff2 := myPackageSet againstBase: imagePackageSet2.
	self assert: diff2 isEmpty