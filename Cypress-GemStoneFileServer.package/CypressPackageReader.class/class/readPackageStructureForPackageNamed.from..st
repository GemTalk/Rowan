unknown
readPackageStructureForPackageNamed: packageName from: aDirectory

	^self readPackageStructureFrom: aDirectory, packageName, '.package'