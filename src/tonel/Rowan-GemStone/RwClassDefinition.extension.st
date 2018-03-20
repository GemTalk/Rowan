Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-gemstone' }
RwClassDefinition >> _modificationForcingNewClassVersion [

	^ RwClassModificationForcingNewClassVersion
]

{ #category : '*rowan-gemstone' }
RwClassDefinition >> compareAgainstBaseForcingNewClassVersion: aDefinition [

	"all unchanged and unremoved methods need to be applied to the patch"

	| modification instanceMethodsModification classMethodsModification |
	modification := self _modificationForcingNewClassVersion
		before: aDefinition
		after: self.
	instanceMethodsModification := self _methodsModificationClass new.
	classMethodsModification := self _methodsModificationClass new.
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

{ #category : '*rowan-gemstone' }
RwClassDefinition >> gs_options: optionsArray [

	(optionsArray _isArray and: [ optionsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: 'gs_options'
				put:
					(optionsArray collect: [ :each | each asString ]) asSortedCollection asArray ]
]
