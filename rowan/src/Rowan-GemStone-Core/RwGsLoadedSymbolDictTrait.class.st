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
RwGsLoadedSymbolDictTrait >> updatePropertiesFromTraitFor: aSymbolDictionaryRegistry [
	"Update my properties from the class. Leave unknown properties alone, they may apply to another platform."

	self
		updateSymbolDictionaryFromTraitFor:
			aSymbolDictionaryRegistry _symbolDictionary
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTrait >> updateSymbolDictionaryFromTraitFor: aSymbolDictionary [

	| newName oldName |
	newName := aSymbolDictionary name.
	oldName := self propertyAt: #'gs_SymbolDictionary'.	"needs to be listed in _traitsBasedProperties method?"
	oldName = newName
		ifFalse: [ self symbolDictionaryName: newName ]
]
