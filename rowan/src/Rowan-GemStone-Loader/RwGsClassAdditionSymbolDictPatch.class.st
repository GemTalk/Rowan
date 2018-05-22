"
No class-specific documentation for CypGsClassAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassAdditionPatch( classDefinition)

"
Class {
	#name : 'RwGsClassAdditionSymbolDictPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'newClass',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'actions' }
RwGsClassAdditionSymbolDictPatch >> addClassToSymbolDictionary [

	self symbolDictionaryRegistry
		addClassToSymbolDictionaryForClassAssociation: symbolAssociation 
		forClass: newClass
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'actions' }
RwGsClassAdditionSymbolDictPatch >> createAndInstallLoadedClass [

	self symbolDictionaryRegistry
		createAndInstallLoadedClassForClass: newClass 
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'actions' }
RwGsClassAdditionSymbolDictPatch >> createClassFor: aPatchSet [
	newClass := super createClassFor: aPatchSet.
	symbolAssociation := aPatchSet tempAssociationFor: newClass name.
	^ newClass
]

{ #category : 'actions' }
RwGsClassAdditionSymbolDictPatch >> installClassInSystem [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	self symbolDictionaryRegistry
		addClassAssociation: symbolAssociation
		forClass: newClass
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'accessing' }
RwGsClassAdditionSymbolDictPatch >> newClass [
	^newClass

]

{ #category : 'versioning' }
RwGsClassAdditionSymbolDictPatch >> oldClassVersion [
	"We are adding a new class, so there is no old version."

	^nil
]
