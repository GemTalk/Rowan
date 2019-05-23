private
_loadedPackageNamed: packageName ifAbsent: absentBlock

	^ Rowan image loadedPackageNamed: packageName ifAbsent: absentBlock