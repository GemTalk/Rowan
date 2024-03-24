Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	(#(#'_gsFileOffset' #'_gsFileName') includesIdentical: propertyKey)
		ifTrue: [ 
			"_gsFileOffset' '_gsFileName' are informational properties so any differences can be ignored "
			^ true ].
	propertyKey == #'comment'
		ifTrue: [ 
			propertyValue = baseValue
				ifTrue: [ ^ true ]
				ifFalse: [ 
					"empty or nil comments need to compare equal in GemStone"
					^ (propertyValue == nil or: [ propertyValue isEmpty ])
						and: [ baseValue == nil or: [ baseValue isEmpty ] ] ] ].
	propertyKey == #'classvars'
		ifTrue: [ 
			propertyValue = baseValue
				ifTrue: [ ^ true ]
				ifFalse: [ 
					"order is not relevant when comparing class vars"
					(propertyValue _isArray and: [ baseValue _isArray ])
						ifTrue: [ ^ propertyValue sort = baseValue sort ] ] ].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
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
				at: #'gs_constraints'
				ifAbsent: [ #() ]

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_constraints: constraintsArray [

	(constraintsArray _isArray and: [ constraintsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: #'gs_constraints'
				put: constraintsArray ]
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_options [

	^self properties
				at: #'gs_options'
				ifAbsent: [ #() ]

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_options: optionsArray [
	"preserver optionsArray order"

	(optionsArray _isArray and: [ optionsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: #'gs_options'
				put:
					(optionsArray collect: [ :each | each asString ]) asArray ]
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_reservedOop [

	^self properties
				at: #'gs_reservedoop'
				ifAbsent: [ '' ]
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_reservedOop: aString [
	aString isEmpty not
		ifTrue: [ self properties at: #'gs_reservedoop' put: aString ]
]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_symbolDictionary [

	^ self properties
		at: #'gs_SymbolDictionary'
		ifAbsent: []

]

{ #category : '*rowan-gemstone-definitions' }
RwClassDefinition >> gs_symbolDictionary: aSymbolDictionaryName [

	self properties
		at: #'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString

]
