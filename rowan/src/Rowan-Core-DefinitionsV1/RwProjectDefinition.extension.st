Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-core-definitionsv1' }
RwProjectDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	({ #'projectOwnerId' . #'spec'. RwLoadedProject _projectDefinitionSourceKey. #'projectRef' } includesIdentical: propertyKey)
		ifTrue: [ 
		"projectRef entries are considered to be equal for comparison purposes"
		"spec entries are considered to be equal for comparison purposes"
		"_projectDefinitionSourceKey entries are considered equal for comparison purpposes"
		"projectOwnerId entries are considered equal for comparison purpposes"
		^ true ].
	^ super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue
]

{ #category : '*rowan-core-definitionsv1' }
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

{ #category : '*rowan-core-definitionsv1' }
RwProjectDefinition >> projectDefinitionSourceProperty [

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]
]

{ #category : '*rowan-core-definitionsv1' }
RwProjectDefinition >> projectDefinitionSourceProperty: sourceKeyOrNil [

	sourceKeyOrNil ifNil: [ ^ properties removeKey: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [] ].
	^ properties at: RwLoadedProject _projectDefinitionSourceKey put: sourceKeyOrNil
]
