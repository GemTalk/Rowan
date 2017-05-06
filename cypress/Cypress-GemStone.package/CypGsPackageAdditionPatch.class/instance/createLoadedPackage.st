"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: applying
"
createLoadedPackage

	| packageName existingLoadedPackage newLoadedPackage |
	packageName := packageDefinition key.
	existingLoadedPackage := CypGsLoadedPackage
				existingPackageNamed: packageName
				ifAbsent: [nil].
	existingLoadedPackage
		ifNotNil: 
			[self error: 'Internal error -- Attempt to add a package that exists.'].
	newLoadedPackage := CypGsLoadedPackage newNamed: packageName.
	self updatePackageProperties