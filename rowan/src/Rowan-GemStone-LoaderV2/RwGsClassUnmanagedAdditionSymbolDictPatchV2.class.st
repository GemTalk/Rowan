Class {
	#name : 'RwGsClassUnmanagedAdditionSymbolDictPatchV2',
	#superclass : 'RwGsClassAdditionSymbolDictPatchV2',
	#instVars : [
		'oldClassVersion'
	],
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
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> installClassInSystem [
	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedClass |
	[ 
	loadedClass := self symbolDictionaryRegistry
		addClassAssociation: symbolAssociation
		forClass: newClass
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2 ]
		on: RwExistingAssociationWithSameKeyNotification
		do: [ :ex | ex resume ].
	loadedClass updatePropertiesFromClassDefinition: self classDefinition
]

{ #category : 'actions' }
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> installNewClassVersionInSystem [
	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	^ self installClassInSystem
]

{ #category : 'accessing' }
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> oldClassVersion [
	^oldClassVersion
]

{ #category : 'accessing' }
RwGsClassUnmanagedAdditionSymbolDictPatchV2 >> oldClassVersion: object [
	oldClassVersion := object
]
