Class {
	#name : 'RwGSLoadedSymbolDictPackage',
	#superclass : 'RwLoadedPackage',
	#category : 'Rowan-GemStone'
}

{ #category : 'queries' }
RwGSLoadedSymbolDictPackage >> classOrExtensionForClass: behavior ifAbsent: absentBlock [

	| className |
	className := behavior theNonMetaClass name asString.
	^ loadedClasses
		at: className
		ifAbsent: [ loadedClassExtensions at: className ifAbsent: absentBlock ]
]

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
RwGSLoadedSymbolDictPackage >> loadedClassExtensionForClass: aClass ifAbsent: absentBlock [

	"If there is an existing extension for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^ loadedClassExtensions
		at: aClass theNonMetaClass name asString
		ifAbsent: absentBlock
]

{ #category : 'queries' }
RwGSLoadedSymbolDictPackage >> loadedClassForClass: aClass ifAbsent: absentBlock [

	"If there is an existing loadedClass for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^ loadedClasses at: aClass theNonMetaClass name asString ifAbsent: absentBlock
]
