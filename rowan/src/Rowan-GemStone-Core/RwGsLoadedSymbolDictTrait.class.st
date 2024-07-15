Class {
	#name : 'RwGsLoadedSymbolDictTrait',
	#superclass : 'RwLoadedTrait',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictTrait class >> newForTrait: aTrait [

	^self new initializeForTrait: aTrait
]

{ #category : 'methods' }
RwGsLoadedSymbolDictTrait >> addLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
RwGsLoadedSymbolDictTrait >> addLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'method' }
RwGsLoadedSymbolDictTrait >> addLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self addLoadedClassMethod: aLoadedMethod]
		ifFalse: [self addLoadedInstanceMethod: aLoadedMethod]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> handleTraitDeletion [
	"The Trait to which I refer has been deleted, so I must unregister myself."

	self loadedPackage removeLoadedTrait: self
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictTrait >> initialize [

	super initialize.
	loadedInstanceMethods := StringKeyValueDictionary new.
	loadedClassMethods := StringKeyValueDictionary new
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictTrait >> initializeForName: aTrait [

	super initializeForName: aTrait.
	properties at: #'name' put: aTrait
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictTrait >> initializeForTrait: aTrait [

	self initialize.
	self initializeForName: aTrait name asString.
	handle := aTrait
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictTrait >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedClass in the same package."

	^name
]

{ #category : 'removing' }
RwGsLoadedSymbolDictTrait >> removeFromLoaded: aPackageSymbolDictionary [

	self disownFromLoaded: aPackageSymbolDictionary.
	self handleTraitDeletion
]

{ #category : 'methods' }
RwGsLoadedSymbolDictTrait >> removeLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in LoadedTrait ', self key printString]
]

{ #category : 'methods' }
RwGsLoadedSymbolDictTrait >> removeLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in LoadedClassExtension']
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictTrait >> symbolDictionaryName [

	^self propertyAt: #'gs_SymbolDictionary'
]

{ #category : 'private' }
RwGsLoadedSymbolDictTrait >> symbolDictionaryName: aName [

	self propertyAt: #'gs_SymbolDictionary' put: aName asString
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updateCategoryFromTrait [

	| propertyName oldValue newValue absentToken |
	propertyName := #'category'.	"needs to be listed in _traitBasedProperties method"
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	newValue := handle category ifNil: [ absentToken ].
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
RwGsLoadedSymbolDictTrait >> updateClassInstvarNamesFromTrait [

	| propertyName oldNames newNames |
	propertyName := #'classinstvars'.	"needs to be listed in _traitBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := handle classTrait instVarNames collect: [:each | each asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updateClassVariableNamesFromTrait [
	"Order of variable names does not matter to for GemStone so we keep the names sorted."

	| propertyName oldNames newNames |
	propertyName := #'classvars'.	"needs to be listed in _traitBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := (handle classVarNames collect: [:each | each asString])
				asSortedCollection asArray.
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updateCommentFromTrait [

	| propertyName oldValue newValue absentToken |
	propertyName := #'comment'.	"needs to be listed in _traitBasedProperties method"
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	(oldValue ~~ absentToken and: [oldValue isEmpty])
		ifTrue: [oldValue := absentToken].
	newValue := (handle comment) ifNil: [absentToken].
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
RwGsLoadedSymbolDictTrait >> updateInstvarNamesFromTrait [

	| propertyName oldNames newNames |
	propertyName := #'instvars'.	"needs to be listed in _traitBasedProperties method"
	oldNames := self propertyAt: propertyName.
	newNames := handle instVarNames collect: [:each | each asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updatePropertiesFromTraitFor: aSymbolDictionaryRegistry [
	"Update my properties from the trait. Leave unknown properties alone, they may apply to another platform."

	self
		updateSymbolDictionaryFromTraitFor:
			aSymbolDictionaryRegistry _symbolDictionary;
		updateTraitNameFromTrait;
		updateInstvarNamesFromTrait;
		updateClassInstvarNamesFromTrait;
		updateClassVariableNamesFromTrait;
		updateCommentFromTrait;
		updateCategoryFromTrait
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updateSymbolDictionaryFromTraitFor: aSymbolDictionary [

	| newName oldName |
	newName := aSymbolDictionary name.
	oldName := self propertyAt: #'gs_SymbolDictionary'.	"needs to be listed in _traitsBasedProperties method?"
	oldName = newName
		ifFalse: [ self symbolDictionaryName: newName ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updateTraitNameFromTrait [

	| oldName newName |
	oldName := name.
	newName := handle name asString.
	oldName = newName
		ifFalse: 
			[self name: newName.
			self propertyAt: #'name' put: name] 	"needs to be listed in _traitBasedProperties method"
]
