validation
_validateRowanHybridClassCategoryConvention: aClassDefinition forPackageNamed: packageName

	aClassDefinition category = packageName ifTrue: [ ^ self ].
	RwInvalidClassCategoryConventionErrorNotification 
		signalWithClassDefinition: aClassDefinition 
			packageName: packageName 
			packageConvention: 'RowanHybrid'