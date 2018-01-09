"
No class-specific documentation for CypGsClassVersioningPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVersioningPatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVersioningPatch',
	#superclass : 'RwGsClassCreationPatch',
	#instVars : [
		'oldClassDefinition',
		'oldClassVersion',
		'newClassVersion',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'patching' }
RwGsClassVersioningPatch class >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ (self new)
		addPatchedClassModification: aClassModification
		inPackage: aPackageDefinition 
		toPatchSet: aRwGsPatchSet

]

{ #category : 'instance creation' }
RwGsClassVersioningPatch class >> for: aClassModification inPackage: aPackageDefinition [

	^(self new)
		oldClassDefinition: aClassModification before;
		classDefinition: aClassModification after;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'patching' }
RwGsClassVersioningPatch >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	| dictName dict existingClass  newOpts newFormat aClassDefinition |
	aClassDefinition := aClassModification after.
	dictName := aClassDefinition propertyAt: 'gs_SymbolDictionary'
				ifAbsent: ['UserGlobals'].
	dict := self existingSymbolDictionaryNamed: dictName.
	dict ifNil: [ self error: 'internal error - symbolDictionary named: ', dictName printString, ' not found' ].
	existingClass := dict 
		at: aClassDefinition name asSymbol
		ifAbsent: [
			self error: 'Internal error. Attempt to modify a class whose name is not bound.' ].
	newOpts := (aClassDefinition propertyAt: 'gs_options' ifAbsent: [ #() ]) 
					copy 
					add: #logCreation;
					yourself.
	newFormat := self _classFormat: existingClass superclass format forSubclassType: aClassDefinition classType.
	(existingClass superclass 
		_equivalentSubclass: existingClass 
		superCls:existingClass superclass
 		name: aClassDefinition name
		newOpts: newOpts
		newFormat: newFormat
		newInstVars: aClassDefinition instVarNames
		newClassInstVars: aClassDefinition classInstVarNames
		newPools: aClassDefinition poolDictionaryNames
		newClassVars:aClassDefinition classVarNames
		inDict: dict
		isKernel: false "needs work .. maybe")
			ifTrue: [ aRwGsPatchSet addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition ]
			ifFalse: [ aRwGsPatchSet addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition ]
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> addToNewClassesByName: aDictionary [
	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name |
	name := classDefinition key.
	name ifNil: [self error: 'Class definition with no name.'].
	(aDictionary includesKey: name) ifTrue: [self error: 'Duplicate name'].
	aDictionary at: name put: self
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> createClassFor: aPatchSet [

	oldClassVersion := self oldClassVersion.
	newClassVersion := super createClassFor: aPatchSet.
	symbolAssociation := aPatchSet associationFor: newClassVersion name.
	^newClassVersion
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> installNewClassVerionInSystem [
	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	self
		updateSymbolAssociation;
		updateLoadedClass
]

{ #category : 'accessing' }
RwGsClassVersioningPatch >> oldClassDefinition [

   ^oldClassDefinition

]

{ #category : 'accessing' }
RwGsClassVersioningPatch >> oldClassDefinition: anObject [

   oldClassDefinition := anObject

]

{ #category : 'versioning' }
RwGsClassVersioningPatch >> oldClassVersion [
	"The old version is what is currently bound to the old class definition's name."

	^(self resolveName: oldClassDefinition key) value
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> updateLoadedClass [
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> updateSymbolAssociation [
]
