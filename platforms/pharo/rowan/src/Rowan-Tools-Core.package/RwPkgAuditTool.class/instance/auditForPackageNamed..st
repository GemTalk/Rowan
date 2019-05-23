other
auditForPackageNamed: packageName
	
	^self auditForPackage: (Rowan image loadedPackageNamed: packageName).
