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
	#category : 'Rowan-GemStone'
}

{ #category : 'actions' }
RwGsClassAdditionPatch >> addClassToSymbolDictionary [
	"Add the name association to the correct 
        SymbolDictionary in the live SymbolList."

	| dictName dict existingAssociation |
	dictName := classDefinition propertyAt: 'gs_SymbolDictionary'
				ifAbsent: ['UserGlobals'].
	dict := self newOrExistingSymbolDictionaryNamed: dictName.
	existingAssociation := dict associationAt: newClass name ifAbsent: [nil].
	existingAssociation
		ifNotNil: 
			[self
				error: 'Internal error. Attempt to install class whose name is already bound.'].
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

{ #category : 'actions' }
RwGsClassAdditionPatch >> newOrExistingSymbolDictionaryNamed: dictName [

	"If the current session's transient symbol list includes a dictionary with the given name, answer it.
	Otherwise, create one, add it to the end of both transient and persistent symbol lists, and answer it."

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^ symbolList
		detect: [ :each | (each at: symbolName ifAbsent: [ nil ]) == each ]
		ifNone: [ 
			| newDict |
			newDict := SymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			symbolList addLast: newDict.
			session userProfile symbolList addLast: newDict.
			newDict ]
]

{ #category : 'versioning' }
RwGsClassAdditionPatch >> oldClassVersion [
	"We are adding a new class, so there is no old version."

	^nil
]
