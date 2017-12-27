"
Keeps track of what LoadedPackages are in the GemStone repository, and is the gateway for operations to query or change the image.
"
Class {
	#name : 'CypGsImage',
	#superclass : 'Object',
	#classInstVars : [
		'PackageRegistry'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'modifying image' }
CypGsImage class >> applyModification: aPackageSetModification [

	| visitor patchSet |
	visitor := CypGsImagePatchVisitor new.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet apply
]
