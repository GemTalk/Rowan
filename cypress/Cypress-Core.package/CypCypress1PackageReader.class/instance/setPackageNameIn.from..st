"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
setPackageNameIn: packageDefinition from: aPath
	"The Cypress reference implementation incorrectly omits name from the package properties. 
	So if the name is not set, get it from the directory name."

	packageDefinition propertyAt: 'name'
		ifAbsent: 
			[| packageName dirName expectedExtension |
			dirName := disk localNameFrom: aPath.
			expectedExtension := '.package'.
			(dirName endsWith: expectedExtension)
				ifFalse: 
					[self
						error: 'Package directory does not have a valid name for Cypress1 format.'].
			packageName := dirName copyFrom: 1
						to: dirName size - expectedExtension size.
			packageDefinition propertyAt: 'name' put: packageName]