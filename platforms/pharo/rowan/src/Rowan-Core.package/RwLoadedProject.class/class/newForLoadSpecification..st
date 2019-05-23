instance creation
newForLoadSpecification: aLoadSpecification

	^ self new
		initialize;
		initializeForLoadSpecification: aLoadSpecification;
		yourself