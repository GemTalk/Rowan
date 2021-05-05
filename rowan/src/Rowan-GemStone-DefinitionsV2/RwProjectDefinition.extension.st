Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	({#'projectOwnerId'.
	#'spec'.
	(RwLoadedProject _projectDefinitionSourceKey).
	(RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey).
	(RwLoadedProject _projectDefinitionCustomConditionalAttributesKey).
	#'projectRef' } includesIdentical: propertyKey)
		ifTrue: [ 
			"projectRef entries are considered to be equal for comparison purposes"
			"spec entries are considered to be equal for comparison purposes"
			"_projectDefinitionSourceKey entries are considered equal for comparison purpposes"
			"_projectDefinitionPlatformConditionalAttributesKey entries are considered equal for comparison purpposes"
			"projectOwnerId entries are considered equal for comparison purpposes"
			^ true ].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinition >> compareAgainstBase: aDefinition [

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: self packagesForCompare
		againstBaseDictionary: aDefinition packagesForCompare
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinition >> projectDefinitionSourceProperty [

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinition >> projectDefinitionSourceProperty: sourceKeyOrNil [

	sourceKeyOrNil ifNil: [ ^ properties removeKey: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [] ].
	^ properties at: RwLoadedProject _projectDefinitionSourceKey put: sourceKeyOrNil
]
