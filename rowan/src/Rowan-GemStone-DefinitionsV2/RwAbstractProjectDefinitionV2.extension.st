Extension { #name : 'RwAbstractProjectDefinitionV2' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwAbstractProjectDefinitionV2 >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	({'projectOwnerId'.
	'spec'.
	(RwLoadedProject _projectDefinitionSourceKey).
	(RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey).
	'projectRef'} includes: propertyKey)
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
RwAbstractProjectDefinitionV2 >> compareAgainstBase: aDefinition [

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

{ #category : '*rowan-gemstone-definitionsv2' }
RwAbstractProjectDefinitionV2 >> projectDefinitionSourceProperty [

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwAbstractProjectDefinitionV2 >> projectDefinitionSourceProperty: sourceKeyOrNil [

	sourceKeyOrNil ifNil: [ ^ properties removeKey: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [] ].
	^ properties at: RwLoadedProject _projectDefinitionSourceKey put: sourceKeyOrNil
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwAbstractProjectDefinitionV2 >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistry
]
