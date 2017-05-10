"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
createAndInstallLoadedClass
	"Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedClass loadedPackage |
	loadedClass := CypGsLoadedClass newForClass: newClass.
	loadedClass setPropertiesTo: classDefinition properties.
	loadedPackage := CypGsLoadedPackage
				existingPackageNamed: packageDefinition key
				ifAbsent: 
					[self
						error: 'Internal error -- Attempt to add a class to a nonexistent package.'].
	loadedPackage addLoadedClass: loadedClass