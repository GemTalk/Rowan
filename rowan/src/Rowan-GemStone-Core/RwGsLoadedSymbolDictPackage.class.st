Class {
	#name : 'RwGsLoadedSymbolDictPackage',
	#superclass : 'RwLoadedPackage',
	#category : 'Rowan-GemStone-Core'
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

{ #category : 'comparing' }
RwGsLoadedSymbolDictPackage >> gs_symbolDictionary [

	^ self propertiesForCompare
		at: 'gs_SymbolDictionary'
		ifAbsent: []
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictPackage >> initialize [

	super initialize.
	loadedClasses := StringKeyValueDictionary new.
	loadedClassExtensions := StringKeyValueDictionary new.
	loadedTraits := StringKeyValueDictionary new.
	"A new package is dirty until it has been shown to be identical to a repository."
	isDirty := true
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictPackage >> initializeForName: aName [

	super initializeForName: aName.
	properties at: #'name' put: aName
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

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> packageSymbolDictionaryName [

	^self propertyAt: #'gs_SymbolDictionary'
]

{ #category : 'queries' }
RwGsLoadedSymbolDictPackage >> packageSymbolDictionaryName: symDictName [
	self propertyAt: #'gs_SymbolDictionary' put: symDictName asString
]

{ #category : 'private' }
RwGsLoadedSymbolDictPackage >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at:  #'gs_SymbolDictionary' put: (Rowan image loadedRegistryForPackageNamed: self name) _symbolDictionary name asString.
	^ props

]

{ #category : 'private' }
RwGsLoadedSymbolDictPackage >> updatePropertiesFromRegistryFor: aSymbolDictionaryRegistry [
	"Update my properties from the symbol dictionary registry."

	| newName oldName |
	newName := aSymbolDictionaryRegistry _symbolDictionary name.
	oldName := self propertyAt: #'gs_SymbolDictionary'.
	oldName = newName
		ifFalse: [ self packageSymbolDictionaryName: newName ]
]
