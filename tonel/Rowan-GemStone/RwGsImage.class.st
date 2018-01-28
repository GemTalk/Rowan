"
Keeps track of what LoadedPackages are in the GemStone repository, and is the gateway for operations to query or change the image.
"
Class {
	#name : 'RwGsImage',
	#superclass : 'Object',
	#category : 'Rowan-GemStone'
}

{ #category : 'modifying image' }
RwGsImage class >> applyModification: aPackageSetModification for: aPackageSymbolDictionary [

	| visitor patchSet |
	visitor := RwGsImagePatchVisitor newFor: aPackageSymbolDictionary.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	patchSet classesWithNewVersions
		do: [ :each | each newClassVersionPatches: aPackageSetModification patchSet: nil ].
	visitor := RwGsImagePatchVisitor newFor: aPackageSymbolDictionary.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet apply
]
