accessing
addPackagesNamed: packageNames withConditions: conditionArray andGroupName: groupName

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName ]