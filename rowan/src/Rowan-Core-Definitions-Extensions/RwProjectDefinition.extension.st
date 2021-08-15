Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwProjectDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	({ 'spec'. RwLoadedProject _projectDefinitionSourceKey } includes: propertyKey)
		ifTrue: [ 
		"spec entries are considered to be equal for comparison purposes"
		"_projectDefinitionSourceKey entries are considered equal for comparison purpposes"
		^ true ].
	^ super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue
]

{ #category : '*rowan-core-definitions-extensions' }
RwProjectDefinition >> compareAgainstBase_forUpgrade: aDefinition [

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: packages
		againstBaseDictionary_forUpgrade: aDefinition packages
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
]

{ #category : '*rowan-core-definitions-extensions' }
RwProjectDefinition >> compareAgainstBase: aDefinition [

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: packages
		againstBaseDictionary: aDefinition packages
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
]

{ #category : '*rowan-core-definitions-extensions' }
RwProjectDefinition >> projectDefinitionSourceProperty [

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]
]
