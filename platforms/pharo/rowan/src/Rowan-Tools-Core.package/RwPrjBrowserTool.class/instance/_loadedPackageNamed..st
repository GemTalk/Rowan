private
_loadedPackageNamed: packageName

	^ self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ]