Class {
	#name : 'RwGsClassUnmanagedAdditionSymbolDictPatchV2',
	#superclass : 'RwGsClassAdditionSymbolDictPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> createClassFor: aPatchSet [
self error: 'probably shouldn''t be implemented or sent by this class'.
	newClass := super createClassFor: aPatchSet.
	symbolAssociation := aPatchSet tempAssociationFor: newClass name.
	^ newClass
]

{ #category : 'actions' }
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> createClassFor: aPatchSet inSymDict: symDictName [
self halt.
	newClass := super createClassFor: aPatchSet inSymDict: symDictName.
	symbolAssociation := aPatchSet tempAssociationFor: newClass name.
	^ newClass
]

{ #category : 'actions' }
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> installClassInSystem [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedClass |
self halt.
	loadedClass := self symbolDictionaryRegistry
		addClassAssociation: symbolAssociation
		forClass: newClass
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	loadedClass updatePropertiesFromClassDefinition: self classDefinition
]
