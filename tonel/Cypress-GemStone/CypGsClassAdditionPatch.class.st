"
No class-specific documentation for CypGsClassAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassAdditionPatch( classDefinition)

"
Class {
	#name : 'CypGsClassAdditionPatch',
	#superclass : 'CypGsClassCreationPatch',
	#instVars : [
		'newClass',
		'symbolAssociation'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'actions' }
CypGsClassAdditionPatch >> addClassToSymbolDictionary [
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
CypGsClassAdditionPatch >> addToDictionary: aDictionary [
	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name |
	name := classDefinition key.
	name ifNil: [self error: 'Class definition with no name.'].
	(aDictionary includesKey: name) ifTrue: [self error: 'Duplicate name'].
	aDictionary at: name put: self
]

{ #category : 'actions' }
CypGsClassAdditionPatch >> createAndInstallLoadedClass [
	"Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedClass loadedPackage |
	loadedClass := CypGsLoadedClass newForClass: newClass.
	loadedClass setPropertiesTo: classDefinition properties.
	loadedPackage := CypGsLoadedPackage
				existingPackageNamed: packageDefinition key
				ifAbsent: 
					[self
						error: 'Internal error -- Attempt to add a class to a nonexistent package.'].
	loadedPackage addLoadedClass: loadedClass
]

{ #category : 'actions' }
CypGsClassAdditionPatch >> createClassFor: aPatchSet [

	newClass := super createClassFor: aPatchSet.
	symbolAssociation := aPatchSet associationFor: newClass name.
	^newClass
]

{ #category : 'actions' }
CypGsClassAdditionPatch >> installClassInSystem [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	self
		addClassToSymbolDictionary;
		createAndInstallLoadedClass
]

{ #category : 'accessing' }
CypGsClassAdditionPatch >> newClass [
	^newClass

]

{ #category : 'actions' }
CypGsClassAdditionPatch >> newOrExistingSymbolDictionaryNamed: dictName [
	"If the current session's transient symbol list includes a dictionary with the given name, answer it.
	Otherwise, create one, add it to the end of both transient and persistent symbol lists, and answer it."

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^symbolList
		detect: [:each | (each at: symbolName ifAbsent: [nil]) == each]
		ifNone: 
			[| newDict |
			newDict := (SymbolDictionary new)
						name: symbolName;
						objectSecurityPolicy: symbolList objectSecurityPolicy;
						yourself.
			symbolList addLast: newDict.
			session userProfile symbolList addLast: newDict.
			newDict]
]

{ #category : 'versioning' }
CypGsClassAdditionPatch >> oldClassVersion [
	"We are adding a new class, so there is no old version."

	^nil
]
