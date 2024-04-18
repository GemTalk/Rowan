"
Walks a diff tree and adds patches to a patchSet that will update the GemStone image (repository) to the code state represented by the 'after' state of the diff, provided that the 'before' state of the diff is the current state of the image.
"
Class {
	#name : 'RwGsImagePatchVisitor_V2',
	#superclass : 'Object',
	#instVars : [
		'patchSet',
		'currentProject',
		'currentPackage',
		'currentClass',
		'currentTrait',
		'currentMethod'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsImagePatchVisitor_V2 class >> new [

	^super new initialize
]

{ #category : 'private' }
RwGsImagePatchVisitor_V2 >> addAddedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addAddedPackage: currentPackage inProject: currentProject.

]

{ #category : 'private' }
RwGsImagePatchVisitor_V2 >> addAddedProject: aProjectDefinition [

	currentProject := aProjectDefinition.
	patchSet addAddedProject: currentProject
]

{ #category : 'private' }
RwGsImagePatchVisitor_V2 >> addDeletedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addDeletedPackage: currentPackage inProject: currentProject

]

{ #category : 'private' }
RwGsImagePatchVisitor_V2 >> addDeletedProject: aProjecteDefinition [

	self error: 'not yet implemented'
]

{ #category : 'initialization' }
RwGsImagePatchVisitor_V2 >> initialize [

	patchSet := RwGsPatchSet_V2 new
]

{ #category : 'accessing' }
RwGsImagePatchVisitor_V2 >> patchSet [
	^patchSet

]

{ #category : 'public' }
RwGsImagePatchVisitor_V2 >> visit: aPackageSetModification [

	^aPackageSetModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitClassesModification: aClassesModification [

	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitClassExtensionModification: aClassExtensionModification [

	aClassExtensionModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassExtensionModification after.
	aClassExtensionModification instanceMethodsModification acceptVisitor: self.
	aClassExtensionModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitClassExtensionsModification: aClassExtensionsModification [

	aClassExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitClassModification: aClassModification [

	aClassModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitClassModificationForcingNewVersion: aClassModification [

	aClassModification
		addModificationForcingNewClassVersionToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitExtensionMethodModification: anExtensionMethodModification [

	anExtensionMethodModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitExtensionMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addExtensionModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitExtensionMethodsModification: anExtensionMethodsModification [

	anExtensionMethodsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitMethodModification: aMethodModification [

	aMethodModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitPackageModification: aPackageModification [
	| propertiesModification beforePackage |
	aPackageModification isAddition
		ifTrue: [ self addAddedPackage: aPackageModification after ].
	currentPackage := aPackageModification after.
	beforePackage := aPackageModification before.
	aPackageModification isDeletion
		ifTrue: [ 
			self addDeletedPackage: aPackageModification before.
			currentPackage := beforePackage ].
	aPackageModification isModification
		ifTrue: [ 
			propertiesModification := aPackageModification propertiesModification.
			(propertiesModification elementsModified includesKey: #'gs_SymbolDictionary')
				ifTrue: [ 
					currentPackage classDefinitions
						keysAndValuesDo: [ :className :afterClassDef | 
							beforePackage classDefinitions
								at: className
								ifPresent: [ :beforeClassDef | 
									patchSet
										addClassMove:
											(RwClassMove
												classBefore: beforeClassDef
												classAfter: afterClassDef
												packageBefore: beforePackage
												packageAfter: currentPackage
												projectBefore: currentProject
												projectAfter: currentProject) ] ].
					patchSet addMovedPackage: currentPackage inProject: currentProject ] ].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self.
	aPackageModification traitsModification acceptVisitor: self.
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitPackageSetModification: aPackageSetModification [

	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitPackagesModification: aPackagesModification [

	aPackagesModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitProjecteSetModification: aProjectSetModification [

	aProjectSetModification movedPackages do: [:movedPackage | 
		movedPackage addMovedPackageToPatchSet: patchSet ].
	aProjectSetModification movedClasses do: [:movedClass | 
		movedClass addMovedClassToPatchSet: patchSet ].
	aProjectSetModification movedMethods do: [:movedMethod | 
		movedMethod addMovedMethodToPatchSet: patchSet ].
	aProjectSetModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitProjectModification: aProjectModification [

	| propertiesModification |
	aProjectModification isAddition
		ifTrue: [ 
			self addAddedProject: aProjectModification after.
			aProjectModification packagesModification acceptVisitor: self.
			^ self ].
	aProjectModification isDeletion
		ifTrue: [ 
			self addDeletedProject: aProjectModification before.
			aProjectModification packagesModification acceptVisitor: self.
			^ self ].
	currentProject := aProjectModification after.
	propertiesModification := aProjectModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: [ 
			"https://github.com/dalehenrich/Rowan/issues/180"
			| modifiedProperties |
			modifiedProperties := String new.
			propertiesModification elementsModified keys do: [:key | modifiedProperties add: ' '; add: key printString ].
			self error: 'internal error - unexpected non-empty project properties modification:', modifiedProperties, '. See Issue #180'  ].
	aProjectModification packagesModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitPropertiesModification: aPropertiesModification [

	self error: 'internal error - not yet implemented'

]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitSourceModification: aSourceModification [

	self error: 'internal error - not yet implemented'

]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitTraitModification: aTraitModification [

	aTraitModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentTrait := aTraitModification after.
	aTraitModification instanceMethodsModification acceptVisitor: self.
	aTraitModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_V2 >> visitTraitsModification: aTraitsModification [
	true
		ifTrue: [ aTraitsModification elementsModified do: [ :each | each acceptVisitor: self ] ]
]
