accessing
addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: groupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }.
	^package