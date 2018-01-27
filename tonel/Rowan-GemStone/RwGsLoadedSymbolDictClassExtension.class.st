Class {
	#name : 'RwGsLoadedSymbolDictClassExtension',
	#superclass : 'RwLoadedClass',
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictClassExtension class >> newForClass: aClass inPackage: aLoadedPackage [

	^ self new initializeForClass: aClass inPackage: aLoadedPackage
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClassExtension >> addLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClassExtension >> addLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClassExtension >> addLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self addLoadedClassMethod: aLoadedMethod]
		ifFalse: [self addLoadedInstanceMethod: aLoadedMethod]
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictClassExtension >> initialize [

	super initialize.
	loadedInstanceMethods := KeyValueDictionary new.
	loadedClassMethods := KeyValueDictionary new
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictClassExtension >> initializeForClass: aClass inPackage: aLoadedPackage [

	self initialize.
	name := aClass name asString.
	handle := aClass.
	aLoadedPackage addLoadedClassExtension: self.
	self propertyAt: 'name' put: name
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictClassExtension >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedExtendedClass in the same package."

	^name
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClassExtension >> removeLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in LoadedClassExtension']
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClassExtension >> removeLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in LoadedClassExtension']
]

{ #category : 'methods' }
RwGsLoadedSymbolDictClassExtension >> removeLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self removeLoadedClassMethod: aLoadedMethod]
		ifFalse: [self removeLoadedInstanceMethod: aLoadedMethod]
]
