actions
addedPackage: aPackageModification

	currentPackageDefinition := aPackageModification after.

	"create package directory and package.st file"
	self _packageSourceDir ensureCreateDirectory.
	(self _packageSourceDir /  'package.st')
		writeStreamDo: [:aStream |
		aStream 
			<< 'Package ' 
			<< (self _toSTON: { #name ->  currentPackageDefinition name } asDictionary) 
			<< self  _newLine ].

	self processPackage: aPackageModification