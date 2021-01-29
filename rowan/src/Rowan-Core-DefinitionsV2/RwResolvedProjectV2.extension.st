Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-core-definitionsv2' }
RwResolvedProjectV2 >> compareAgainstBase: aDefinition [

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self _projectDefinitionForCompare comparePropertiesAgainstBase: aDefinition _projectDefinitionForCompare).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: self packagesForCompare
		againstBaseDictionary: aDefinition packagesForCompare
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
]

{ #category : '*rowan-core-definitionsv2' }
RwResolvedProjectV2 >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass [

	| keys |
	keys := myDictionary keys. "keys already a copy"
  baseDictionary keysDo:[:k | keys add: k ].  "keys addAll: baseDictionary keys."
	keys do: 
			[:key |
			| before after modification |
			before := baseDictionary at: key ifAbsent: [].
			before := before
				ifNil: [ elementClass new ]
				ifNotNil: [ :def | 
					"at this point convert loaded thing to a real definition"
					def asDefinition ].
			after := myDictionary at: key ifAbsent: [].
			after := after
				ifNil: [ elementClass new ]
				ifNotNil: [ :def | 
					"at this point convert loaded thing to a real definition"
					def asDefinition ].
			modification := after compareAgainstBase: before.
			modification isEmpty
				ifFalse: [anElementsModification addElementModification: modification]]
]
