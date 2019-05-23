accessing
addPackagesNamed: packageNames withConditions: conditionArray

	^ packageNames collect: [:packageName |
			self addPackageNamed: packageName withConditions: conditionArray ]