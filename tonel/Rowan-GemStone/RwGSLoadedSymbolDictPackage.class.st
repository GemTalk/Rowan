Class {
	#name : 'RwGSLoadedSymbolDictPackage',
	#superclass : 'RwLoadedPackage',
	#category : 'Rowan-GemStone'
}

{ #category : 'initialization' }
RwGSLoadedSymbolDictPackage >> initialize [

	super initialize.
	loadedClasses := KeyValueDictionary new.
	loadedClassExtensions := KeyValueDictionary new.
	"A new package is dirty until it has been shown to be identical to a repository."
	isDirty := true
]

{ #category : 'initialization' }
RwGSLoadedSymbolDictPackage >> initializeForName: aName [

	super initializeForName: aName.
	properties at: 'name' put: aName
]

{ #category : 'queries' }
RwGSLoadedSymbolDictPackage >> loadedClassForClass: aClass [

	"If there is an existing loadedClass for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^ loadedClasses at: aClass name asString ifAbsent: [ AbsentToken ]
]

{ #category : 'queries' }
RwGSLoadedSymbolDictPackage >> loadedExtensionForClass: aClass [

	"If there is an existing extension for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^ loadedClassExtensions at: aClass classHistory ifAbsent: [ AbsentToken ]
]
