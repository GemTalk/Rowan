Class {
	#name : 'RwGsLoadedSymbolDictPackage',
	#superclass : 'RwLoadedPackage',
	#category : 'Rowan-GemStone'
}

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> classOrExtensionForClass: behavior ifAbsent: absentBlock [

	| className |
	className := behavior thisClass name asString.
	^ self classOrExtensionForClassNamed: className ifAbsent: absentBlock
]

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> classOrExtensionForClassNamed: className ifAbsent: absentBlock [

	^ loadedClasses
		at: className
		ifAbsent: [ loadedClassExtensions at: className ifAbsent: absentBlock ]
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictPackage >> initialize [

	super initialize.
	loadedClasses := KeyValueDictionary new.
	loadedClassExtensions := KeyValueDictionary new.
	"A new package is dirty until it has been shown to be identical to a repository."
	isDirty := true
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictPackage >> initializeForName: aName [

	super initializeForName: aName.
	properties at: 'name' put: aName
]

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> loadedClassExtensionForClass: aClass ifAbsent: absentBlock [

	"If there is an existing extension for the given (non-meta) class in this package, answer it, otherwise invoke absentBlock."

	^ loadedClassExtensions
		at: aClass thisClass name asString
		ifAbsent: absentBlock
]

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> loadedClassForClass: aClass ifAbsent: absentBlock [

	"If there is an existing loadedClass for the given (non-meta) class in this package, answer it, otherwise invoke absentBlock.."

	^ loadedClasses at: aClass thisClass name asString ifAbsent: absentBlock
]

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> loadedClassOrClassExtensionForClass: aClass ifAbsent: absentBlock [

	"If there is an existing loaded class or loaded class extesnion for the given (non-meta) class in this package, answer it, otherwise invoke absentBlock."

	^ loadedClasses
		at: aClass thisClass name asString
		ifAbsent: [ self loadedClassExtensionForClass: aClass ifAbsent: absentBlock ]
]
