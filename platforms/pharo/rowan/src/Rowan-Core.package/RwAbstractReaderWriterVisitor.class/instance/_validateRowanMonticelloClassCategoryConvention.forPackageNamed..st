validation
_validateRowanMonticelloClassCategoryConvention: aClassDefinition forPackageNamed: packageName

	(aClassDefinition category beginsWith: packageName) ifTrue: [ ^ self ].
	RwInvalidClassCategoryConventionErrorNotification signalWithClassDefinition: aClassDefinition packageName: packageName packageConvention: 'Monticello'