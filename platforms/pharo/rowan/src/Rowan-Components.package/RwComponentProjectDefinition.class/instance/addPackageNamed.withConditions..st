accessing
addPackageNamed: packageName withConditions: conditionArray

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: self defaultGroupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: self defaultGroupName
			addPackageNames: { packageName }.
	^package