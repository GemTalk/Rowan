Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	propertyKey = 'comment' ifFalse: [ ^super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue ].
	propertyValue = baseValue
		ifTrue: [ ^ true ]
		ifFalse: [ 
			"empty or nil comments need to compare equal in GemStone"
			^(propertyValue == nil or: [ propertyValue isEmpty]) and: [ baseValue == nil or: [ baseValue isEmpty] ] ]

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> _modificationForcingNewClassVersion [

	^ RwClassModificationForcingNewClassVersion
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> compareAgainstBaseForcingNewClassVersion: aDefinition [

	"all unchanged and unremoved methods need to be applied to the patch"

	| modification instanceMethodsModification classMethodsModification |
	modification := self _modificationForcingNewClassVersion
		before: aDefinition
		after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	instanceMethodsModification := self _methodsModificationClass
		extendedClassName: self key.
	classMethodsModification := self _methodsModificationClass
		extendedClassName: self key.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: RwMethodDefinition
		isMeta: false.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: RwMethodDefinition
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_constraints [

	^self properties
				at: 'gs_constraints'
				ifAbsent: [ #() ]

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_constraints: constraintsArray [

	constraintsArray _isArray
		ifTrue: [ 
			constraintsArray isEmpty
				ifTrue: [ 
					self properties
						removeKey: 'gs_constraints'
						ifAbsent: [] ]
				ifFalse: [ 
					self properties
						at: 'gs_constraints'
						put: constraintsArray ] ]
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_options [

	^self properties
				at: 'gs_options'
				ifAbsent: [ #() ]

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_options: optionsArray [

	(optionsArray _isArray and: [ optionsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: 'gs_options'
				put:
					(optionsArray collect: [ :each | each asString ]) asSortedCollection asArray ]
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_symbolDictionary [

	^ self properties
		at: 'gs_SymbolDictionary'
		ifAbsent: []

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_symbolDictionary: aSymbolDictionaryName [

	self properties
		at: 'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString

]
