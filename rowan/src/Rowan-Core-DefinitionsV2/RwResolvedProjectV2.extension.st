Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-core-definitionsv2' }
RwResolvedProjectV2 >> compareAgainstBase: aDefinition [
	"return a project modification sufficient to transform aDefinition into the reciever"

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self _projectDefinition comparePropertiesAgainstBase: aDefinition _projectDefinition).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: self packages
		againstBaseDictionary: aDefinition packages
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
]

{ #category : '*rowan-core-definitionsv2' }
RwResolvedProjectV2 >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass [
	"create a projectSetModification sufficient to transform definitions in baseDictionary into the definitions in myDictionary"

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after modification |
			before := baseDictionary at: key ifAbsent: [elementClass new].
			after := myDictionary at: key ifAbsent: [elementClass new].
			modification := after compareAgainstBase: before.
			modification isEmpty
				ifFalse: [anElementsModification addElementModification: modification]]
]
