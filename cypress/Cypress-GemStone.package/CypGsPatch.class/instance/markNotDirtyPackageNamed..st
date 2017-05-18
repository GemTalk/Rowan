"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: packages
"
markNotDirtyPackageNamed: packageName

	| loadedPackage |
	loadedPackage := Cypress loadedPackageNamed: packageName
				ifAbsent: 
					[self error: 'Internal error -- attempt to manipulate nonexistent package.'].
	loadedPackage markNotDirty