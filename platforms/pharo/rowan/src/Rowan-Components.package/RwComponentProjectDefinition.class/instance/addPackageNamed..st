accessing
addPackageNamed: packageName

	^ self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: #( 'common' ) 
			andGroupName: self defaultGroupName