accessing
addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName


	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName ]