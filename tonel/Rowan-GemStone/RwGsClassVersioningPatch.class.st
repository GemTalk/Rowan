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
		'newClassVersion'
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
			ifFalse: [ aRwGsPatchSet addPatchedClassNewVersion: aClassDefinition inPackage: aPackageDefinition ]
]

{ #category : 'versioning' }
RwGsClassVersioningPatch >> oldClassVersion [
	"The old version is what is currently bound to the old class definition's name."

	^self resolveName: oldClassDefinition key
]
