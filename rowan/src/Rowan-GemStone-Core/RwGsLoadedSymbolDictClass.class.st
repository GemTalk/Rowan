Class {
	#name : 'RwGsLoadedSymbolDictClass',
	#superclass : 'RwLoadedClass',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictClass class >> newForClass: aClass [

	^self new initializeForClass: aClass
]

{ #category : 'private' }
RwGsLoadedSymbolDictClass >> _classBasedProperties [

	"Answer the list of properties that are derived from the class instance itself"

	^ #(  #'category' #'classinstvars'  #'name' #'type' #'classvars' #'comment' #'gs_constraints' #'instvars'  #'gs_options' 
			#'pools' #'superclass' #'gs_SymbolDictionary' #traits #classTraits)
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClass >> addLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClass >> addLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClass >> addLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self addLoadedClassMethod: aLoadedMethod]
		ifFalse: [self addLoadedInstanceMethod: aLoadedMethod]
]

{ #category : 'comparing' }
RwGsLoadedSymbolDictClass >> gs_symbolDictionary [

	^ self propertiesForCompare
		at: 'gs_SymbolDictionary'
		ifAbsent: []
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> handleClassDeletion [

	"The class to which I refer has been deleted, so I must unregister myself.
	Normally, I will have a loadedPackage, but if something interrupted my initialization it might be nil."

	GsPackagePolicy currentOrNil
		ifNotNil: [ :pp | 
			pp
				removeAllMethodsFor: handle;
				removeAllMethodsFor: handle class ].
	(Rowan image loadedClassExtensionsForClass: handle)
		do: [ :loadedClassExtension | loadedClassExtension handleClassDeletion ].
	Rowan image removeLoadedClassExtensionsForClass: handle.
	self loadedPackage removeLoadedClass: self
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> handleClassDeletionOrNewVersion [

	"The class is deleted if both:
	* its name no longer resolves to a class in the classHistory of the class I remember.
	* no class in the classHistory of the class I remember is accessible under its name.
	If my name no longer resolves to my class, but another class in its classHistory does resolve,
	consider it a class re-version or rename.
	Answer true if the class still exists, false if it has been deleted."

	| resolved thoseResolving |
	resolved := Rowan image resolveClassNamed: name.
	resolved == handle
		ifTrue: [ ^ true ].	"Has been deleted, renamed, or a new version under the same name.
	Answer false if deleted, set handle and answer true otherwise."
	thoseResolving := handle classHistory reverse
		select: [ :aClass | aClass == (Rowan image resolveClassNamed: aClass name) ].
	thoseResolving size = 0
		ifTrue: [ 
			self handleClassDeletion.
			^ false ].
	thoseResolving size > 1
		ifTrue: [ 
			RwNotification
				signal:
					'Class history with multiple "current" members found. One associated name is '
						, name ].
	handle := thoseResolving first.
	^ true
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictClass >> initialize [

	super initialize.
	loadedInstanceMethods := StringKeyValueDictionary new.
	loadedClassMethods := StringKeyValueDictionary new
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictClass >> initializeForClass: aClass [

	self initialize.
	self initializeForName: aClass name asString.
	handle := aClass
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictClass >> initializeForName: aClassName [

	super initializeForName: aClassName.
	properties at: #'name' put: aClassName
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictClass >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedClass in the same package."

	^name
]

{ #category : 'removing' }
RwGsLoadedSymbolDictClass >> removeFromLoaded: aPackageSymbolDictionary [

	self disownFromLoaded: aPackageSymbolDictionary.
	self handleClassDeletion

]

{ #category : 'methods' }
RwGsLoadedSymbolDictClass >> removeLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in loadedClass ', self key printString]
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClass >> removeLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in loadedClass']
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClass >> removeLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self removeLoadedClassMethod: aLoadedMethod]
		ifFalse: [self removeLoadedInstanceMethod: aLoadedMethod]
]

{ #category : 'private' }
RwGsLoadedSymbolDictClass >> reregisterLoadedClassMethods: aDictionary [
	"transfer the loaded methods in aDictionary to the receiver"

	aDictionary do: [ :loadedMethod | loadedMethod loadedClass: self ].
	loadedClassMethods := aDictionary
]

{ #category : 'private' }
RwGsLoadedSymbolDictClass >> reregisterLoadedInstanceMethods: aDictionary [
	"transfer the loaded methods in aDictionary to the receiver"

	aDictionary do: [ :loadedMethod | loadedMethod loadedClass: self ].
	loadedInstanceMethods := aDictionary
]

{ #category : 'private' }
RwGsLoadedSymbolDictClass >> symbolDictionaryName: aName [

	self propertyAt: #'gs_SymbolDictionary' put: aName asString
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateCategoryFromClass [

	| propertyName oldValue newValue absentToken |
	propertyName := #'category'.	"needs to be listed in _classBasedProperties method"
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	newValue := handle _classCategory ifNil: [ absentToken ].
	(newValue ~~ absentToken and: [ newValue isEmpty ])
		ifTrue: [ newValue := absentToken ].	"Absent token must be compared by identity, because a comment of '<absent>' would be equal to no comment."
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [ oldValue == newValue ]
		ifFalse: [ oldValue = newValue ])
		ifFalse: [ 
			newValue == absentToken
				ifTrue: [ self removeProperty: propertyName ]
				ifFalse: [ self propertyAt: propertyName put: newValue ] ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateClassInstvarNamesFromClass [
	"Order of instvars does not matter to Cypress (at least not in GemStone) so we keep the instvar names sorted."

	| propertyName oldNames newNames |
	propertyName := #'classinstvars'.	"needs to be listed in _classBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := handle class instVarNames collect: [:each | each asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateClassNameFromClass [

	| oldName newName |
	oldName := name.
	newName := handle name asString.
	oldName = newName
		ifFalse: 
			[self name: newName.
			self propertyAt: #'name' put: name] 	"needs to be listed in _classBasedProperties method"
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateClassTypeFromClass [
	"For GemStone, the valid values are:
	byteSubclass
	variable
	normal for non-indexable pointer object"

	"Must be in-synch with RwClassDefinition>>_updateClassTypeFromClass:"

	| propertyName oldValue newValue |
	propertyName := #'type'.	"needs to be listed in _classBasedProperties method"
	oldValue := self propertyAt: propertyName.
	newValue := handle rwClassType.

	oldValue = newValue
		ifFalse: [ self propertyAt: propertyName put: newValue ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateClassVariableNamesFromClass [
	"Order of variable names does not matter to Cypress (at least not for GemStone) so we keep the names sorted."

	| propertyName oldNames newNames |
	propertyName := #'classvars'.	"needs to be listed in _classBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := (handle classVarNames collect: [:each | each asString])
				asSortedCollection asArray.
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateCommentFromClass [

	| propertyName oldValue newValue absentToken |
	propertyName := #'comment'.	"needs to be listed in _classBasedProperties method"
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	(oldValue ~~ absentToken and: [oldValue isEmpty])
		ifTrue: [oldValue := absentToken].
	newValue := (handle _extraDictAt: #comment) ifNil: [absentToken].
	(newValue ~~ absentToken and: [newValue isEmpty])
		ifTrue: [newValue := absentToken].

	"Absent token must be compared by identity, because a comment of '<absent>' would be equal to no comment."
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [oldValue == newValue]
		ifFalse: [oldValue = newValue])
			ifFalse: 
				[newValue == absentToken
					ifTrue: [self removeProperty: propertyName]
					ifFalse: [self propertyAt: propertyName put: newValue]]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateConstraintsFromClass [
	| propertyName oldValue newValue sortedConstraints |
	propertyName := #'gs_constraints'.	"needs to be listed in _classBasedProperties method"
	oldValue := self propertyAt: propertyName.
	sortedConstraints := handle _rwSortedConstraints.
	(sortedConstraints isKindOf: Array ) 
		ifTrue: [
			newValue := (sortedConstraints collect: [:ar | 
					(ar isKindOf: Array )
						ifTrue: [ {(ar at: 1) asString . (ar at: 2) name asString } ]
						ifFalse: [ 
							"varyingConstraint"
							ar name asString ] ]) asArray.
			newValue isEmpty ifTrue: [newValue := self absentToken] ]
		ifFalse: [
			newValue :=  sortedConstraints 
				ifNil: [ self absentToken ]
				ifNotNil: [ sortedConstraints ] ].

	"Since the absent token is not equal to any other valid value, we can skip the identity check in this case."
	oldValue = newValue
		ifFalse: 
			[newValue == self absentToken
				ifTrue: [self removeProperty: propertyName]
				ifFalse: [self propertyAt: propertyName put: newValue]]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateInstvarNamesFromClass [
	"Order of instvars does not matter to Cypress (at least not in GemStone) so we keep the instvar names sorted."

	| propertyName oldNames newNames |
	propertyName := #'instvars'.	"needs to be listed in _classBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := handle instVarNames collect: [:each | each asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateOptionsFromClass [
	"Options are GemStone-specific. If the class has no options, the property is absent.
	Otherwise, the property is a sorted array of strings. The valid values are:

	dbTransient
	disallowGciStore
	instancesInvariant
	instancesNonPersistent
	modifiable
	subclassesDisallowed
	traverseByCallback

	Not all combinations are legal, but we don't have to worry about that here, since we're getting the values
	from the image, and the image is trusted to only have valid values."

	| propertyName oldValue newValue |
	propertyName := #'gs_options'.	"needs to be listed in _classBasedProperties method"
	oldValue := self propertyAt: propertyName.	"TODO, may need changes to result of _rwOptionsArray , see RwClassDefinition >> _updateOptionsFromClass "
	newValue := (handle _rwOptionsArray collect: [ :option | option asString ])
		asArray.
	newValue isEmpty
		ifTrue: [ newValue := self absentToken ].	"Since the absent token is not equal to any other valid value, we can skip the identity check in this case."
	oldValue = newValue
		ifFalse: [ 
			newValue == self absentToken
				ifTrue: [ self removeProperty: propertyName ]
				ifFalse: [ self propertyAt: propertyName put: newValue ] ].
	handle _rwReservedOop
		ifNotNil: [ :resOop | self propertyAt: #'gs_reservedoop' put: resOop asString ]
		ifNil: [ self removeProperty: 'gs_reservedoop' ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updatePoolDictionaryNamesFromClass [
	"Order of pool dictionaries *does* matter in GemStone, so we keep the names in the order given in the image."

	| propertyName oldNames newNames |
	propertyName := #'pools'.	"needs to be listed in _classBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := (handle sharedPools collect: [ :each | each name asString ])
		asArray.
	oldNames = newNames
		ifFalse: [ self propertyAt: propertyName put: newNames ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updatePropertiesFromClassFor: aSymbolDictionaryRegistry [

	"Update my properties from the class. Leave unknown properties alone, they may apply to another platform."
	self
		updateSymbolDictionaryFromClassFor:
				aSymbolDictionaryRegistry _symbolDictionary;
		updateClassNameFromClass;
		updateSuperclassNameFromClass;
		updateInstvarNamesFromClass;
		updateClassInstvarNamesFromClass;
		updateClassVariableNamesFromClass;
		updatePoolDictionaryNamesFromClass;
		updateClassTypeFromClass;
		updateOptionsFromClass;
		updateConstraintsFromClass;
		updateCommentFromClass;
		updateCategoryFromClass;
		updateTraitsFromClass
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateSuperclassNameFromClass [
  | oldName newName |
  oldName := self propertyAt: #'superclass'.	"needs to be listed in _classBasedProperties method"
  newName := handle superclass
    ifNil: [ 'nil' ]
    ifNotNil: [ :superclass | superclass name asString ].
  oldName = newName
    ifFalse: [ self propertyAt: #'superclass' put: newName ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateSymbolDictionaryFromClassFor: aSymbolDictionary [

	| newName oldName |
	newName := aSymbolDictionary name.
	oldName := self propertyAt: #'gs_SymbolDictionary'.	"needs to be listed in _classBasedProperties method"
	oldName = newName
		ifFalse: [ self symbolDictionaryName: newName ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictClass >> updateTraitsFromClass [
	| propertyName oldValue newValue absentToken |
	"
classTraits"
	propertyName := #'classTraits'.	"needs to be listed in _classBasedProperties method"
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	newValue := handle classTraits.	"a collection containing a string describing a single classTrait ... or empty"
	(newValue ~~ absentToken and: [ newValue isEmpty ])
		ifTrue: [ newValue := absentToken ].
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [ oldValue == newValue ]
		ifFalse: [ oldValue = newValue ])
		ifFalse: [ 
			newValue == absentToken
				ifTrue: [ self removeProperty: propertyName ]
				ifFalse: [ self propertyAt: propertyName put: newValue any asString ] ].	"
traits"
	propertyName := #'traits'.	"needs to be listed in _classBasedProperties method"
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	newValue := handle traits.	"a collection containing a string describing a single trait ... or empty"
	(newValue ~~ absentToken and: [ newValue isEmpty ])
		ifTrue: [ newValue := absentToken ].
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [ oldValue == newValue ]
		ifFalse: [ oldValue = newValue ])
		ifFalse: [ 
			newValue == absentToken
				ifTrue: [ self removeProperty: propertyName ]
				ifFalse: [ self propertyAt: propertyName put: newValue any asString ] ]
]
