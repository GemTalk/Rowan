accessing
addPackagesNamed: packageNames

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName ]