"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
setPackageNameIn: packageDefinition from: aPath
	"Sets the name property if unset. Cannot be used to *change* the name of a package."

	| dirName expectedExtension packageNameFromDisk packageNameFromDefinition |
	dirName := disk localNameFrom: aPath.
	expectedExtension := '.package'.
	(dirName endsWith: expectedExtension)
		ifFalse: 
			[self
				error: 'Package directory does not have a valid name for Cypress1 format.'].
	packageNameFromDisk := dirName copyFrom: 1
				to: dirName size - expectedExtension size.
	packageNameFromDefinition := packageDefinition propertyAt: 'name'
				ifAbsent: [nil].
	"If the property is already there, and the same, we're done."
	packageNameFromDisk = packageNameFromDefinition
		ifFalse: 
			[packageNameFromDefinition
				ifNotNil: 
					[self
						error: 'Package directory name and package property file do not agree on the name of the package.'].
			packageDefinition propertyAt: 'name' put: packageNameFromDisk]