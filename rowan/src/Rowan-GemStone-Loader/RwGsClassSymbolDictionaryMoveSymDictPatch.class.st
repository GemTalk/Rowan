Class {
	#name : 'RwGsClassSymbolDictionaryMoveSymDictPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'classModification'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsClassSymbolDictionaryMoveSymDictPatch class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself

]

{ #category : 'accessing' }
RwGsClassSymbolDictionaryMoveSymDictPatch >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after

]

{ #category : 'installing' }
RwGsClassSymbolDictionaryMoveSymDictPatch >> installSymbolDictionaryPatchFor: aPatchSet [

	| before |
	before := classModification before.

self halt.

]
