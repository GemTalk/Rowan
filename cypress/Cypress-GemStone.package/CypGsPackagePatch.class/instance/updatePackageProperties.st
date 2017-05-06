"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: applying
"
updatePackageProperties

	| loadedPackage |
	loadedPackage := CypGsLoadedPackage
				existingPackageNamed: packageDefinition key
				ifAbsent: 
					[self error: 'Internal error. Attempt to update unregistered package.'].
	loadedPackage setPropertiesTo: packageDefinition properties