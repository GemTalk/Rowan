Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-gemstone-core' }
RwClassDefinition >> _modificationForcingNewClassVersion [

	^ RwClassModificationForcingNewClassVersion
]

{ #category : '*rowan-gemstone-core' }
RwClassDefinition >> compareAgainstBaseForcingNewClassVersion: aDefinition [

	"all unchanged and unremoved methods need to be applied to the patch"

	| modification instanceMethodsModification classMethodsModification |
	modification := self _modificationForcingNewClassVersion
		before: aDefinition
		after: self.
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

{ #category : '*rowan-gemstone-core' }
RwClassDefinition >> excludedProperties [
	"return a list of properties that should be ignored when comparing"

	"gs_SymbolDictionary is only used internally for recording the symbol dictionary where a class is installed - ignored for comparison purposes"

	^ #('gs_SymbolDictionary')

]

{ #category : '*rowan-gemstone-core' }
RwClassDefinition >> gs_options: optionsArray [

	(optionsArray _isArray and: [ optionsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: 'gs_options'
				put:
					(optionsArray collect: [ :each | each asString ]) asSortedCollection asArray ]
]
