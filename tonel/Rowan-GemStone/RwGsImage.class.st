"
Keeps track of what LoadedPackages are in the GemStone repository, and is the gateway for operations to query or change the image.
"
Class {
	#name : 'RwGsImage',
	#superclass : 'Object',
	#classInstVars : [
		'PackageRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'modifying image' }
RwGsImage class >> applyModification: aPackageSetModification [
  | visitor patchSet |
  visitor := RwGsImagePatchVisitor new.
  visitor visit: aPackageSetModification.
  patchSet := visitor patchSet.
  patchSet apply
]
