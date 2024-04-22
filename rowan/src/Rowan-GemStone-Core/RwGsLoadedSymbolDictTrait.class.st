Class {
	#name : 'RwGsLoadedSymbolDictTrait',
	#superclass : 'RwLoadedTrait',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictTrait class >> newForTrait: aTrait [

	^self new initializeForTrait: aTrait
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
