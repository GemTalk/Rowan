accessing
addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName

	| package component |
	package := super addPackageNamed: packageName.
	component := self 
		components at: componentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: componentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }.
	^package