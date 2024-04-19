Class {
	#name : 'RwGsClassAdditionSymbolDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#instVars : [
		'newClass',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassAdditionSymbolDictPatchV2 >> createClassFor: aPatchSet inSymDict: symDictName [
	newClass := super createClassFor: aPatchSet inSymDict: symDictName.
	symbolAssociation := aPatchSet tempAssociationFor: newClass name.
	^ newClass
]

{ #category : 'actions' }
RwGsClassAdditionSymbolDictPatchV2 >> installClassInSystem [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedClass |
	loadedClass := self symbolDictionaryRegistry
		addClassAssociation: symbolAssociation
		forClass: self newClass
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	loadedClass updatePropertiesFromClassDefinition: self classDefinition
]

{ #category : 'accessing' }
RwGsClassAdditionSymbolDictPatchV2 >> newClass [
	^newClass

]

{ #category : 'versioning' }
RwGsClassAdditionSymbolDictPatchV2 >> oldClassVersion [
	"We are adding a new class, so there is no old version."

	^nil
]
