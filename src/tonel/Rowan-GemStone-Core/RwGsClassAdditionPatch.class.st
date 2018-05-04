"
No class-specific documentation for CypGsClassAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassAdditionPatch( classDefinition)

"
Class {
	#name : 'RwGsClassAdditionPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'newClass',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'actions' }
RwGsClassAdditionPatch >> addClassToSymbolDictionary [

	"Add the name association to the correct 
        SymbolDictionary in the live SymbolList."

	| dictName dict existingAssociation |
	dictName := classDefinition
		propertyAt: 'gs_SymbolDictionary'
		ifAbsent: [ 'UserGlobals' ].
	dict := Rowan image newOrExistingSymbolDictionaryNamed: dictName.
	existingAssociation := dict associationAt: newClass name ifAbsent: [ nil ].
	existingAssociation
		ifNotNil: [ 
			self
				error:
					'Internal error. Attempt to install class whose name is already bound.' ].
	symbolAssociation value: newClass.
	dict add: symbolAssociation
]

{ #category : 'actions' }
RwGsClassAdditionPatch >> createAndInstallLoadedClass [

	"Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedClass loadedPackage |
	loadedClass := RwGsLoadedClass newForClass: newClass.
	loadedClass setPropertiesTo: classDefinition properties.
	loadedPackage := RwGsLoadedPackage
		existingPackageNamed: self packageName
		ifAbsent: [ self error: 'Internal error -- Attempt to add a class to a nonexistent package.' ].
	loadedPackage addLoadedClass: loadedClass
]

{ #category : 'actions' }
RwGsClassAdditionPatch >> createClassFor: aPatchSet [
	newClass := super createClassFor: aPatchSet.
	symbolAssociation := aPatchSet tempAssociationFor: newClass name.
	^ newClass
]

{ #category : 'actions' }
RwGsClassAdditionPatch >> installClassInSystem [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	self
		addClassToSymbolDictionary;
		createAndInstallLoadedClass
]

{ #category : 'accessing' }
RwGsClassAdditionPatch >> newClass [
	^newClass

]

{ #category : 'versioning' }
RwGsClassAdditionPatch >> oldClassVersion [
	"We are adding a new class, so there is no old version."

	^nil
]
