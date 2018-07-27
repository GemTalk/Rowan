"
Walks a diff tree and adds patches to a patchSet that will update the GemStone image (repository) to the code state represented by the 'after' state of the diff, provided that the 'before' state of the diff is the current state of the image.
"
Class {
	#name : 'RwGsImagePatchVisitor_254',
	#superclass : 'Object',
	#instVars : [
		'patchSet',
		'currentProject',
		'currentPackage',
		'currentClass',
		'currentMethod'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsImagePatchVisitor_254 class >> new [

	^super new initialize
]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> _patchSetClass [

	^ RwGsPatchSet_254
]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> addAddedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addAddedPackage: currentPackage inProject: currentProject.

]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> addAddedProject: aProjectDefinition [

	currentProject := aProjectDefinition.
	patchSet addAddedProject: currentProject
]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> addClasses: classDefinitions [

	classDefinitions
		do: [ :classDefinition | 
			"https://github.com/dalehenrich/Rowan/issues/210 - make sure that the added classes are not already loaded
				in a project that is not included in this load"
			(Rowan globalNamed: classDefinition name) ifNotNil: [:global | (RwExistingVisitorAddingExistingClassNotification new classDefinition: classDefinition) signal ]].
	classDefinitions
		do: [ :classDefinition | 
			patchSet
				addAddedClass: classDefinition
				inPackage: currentPackage
				inProject: currentProject.
			classDefinition classMethodDefinitions
				do: [ :classMethodDefinition | 
					patchSet
						addAddedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage
						inProject: currentProject ].
			classDefinition instanceMethodDefinitions
				do: [ :instanceMethodDefinition | 
					patchSet
						addAddedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage
						inProject: currentProject ] ]

]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> addClassExtensions: classExtensions [

	classExtensions
		do: [ :classDefinition | 
			patchSet
				addExtendedClass: classDefinition
				inPackage: currentPackage
				inProject: currentProject.
			classDefinition classMethodDefinitions
				do: [ :classMethodDefinition | 
					patchSet
						addExtendedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage
						inProject: currentProject ].
			classDefinition instanceMethodDefinitions
				do: [ :instanceMethodDefinition | 
					patchSet
						addExtendedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage
						inProject: currentProject ] ]
]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> addDeletedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addDeletedPackage: currentPackage inProject: currentProject

]

{ #category : 'private' }
RwGsImagePatchVisitor_254 >> deleteClasses: classDefinitions [

	classDefinitions
		do: [ :classDefinition | 
			patchSet
				addDeletedClass: classDefinition
				inPackage: currentPackage
				inProject: currentProject.
			classDefinition classMethodDefinitions
				do: [ :classMethodDefinition | 
					patchSet
						addDeletedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage
						inProject: currentProject ].
			classDefinition instanceMethodDefinitions
				do: [ :instanceMethodDefinition | 
					patchSet
						addDeletedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage
						inProject: currentProject ] ]
]

{ #category : 'initialization' }
RwGsImagePatchVisitor_254 >> initialize [

	patchSet := self _patchSetClass new
]

{ #category : 'accessing' }
RwGsImagePatchVisitor_254 >> patchSet [
	^patchSet

]

{ #category : 'public' }
RwGsImagePatchVisitor_254 >> visit: aPackageSetModification [

	^aPackageSetModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitClassesModification: aClassesModification [

	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitClassExtensionModification: aClassExtensionModification [

	aClassExtensionModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassExtensionModification after.
	aClassExtensionModification instanceMethodsModification acceptVisitor: self.
	aClassExtensionModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitClassExtensionsModification: aClassExtensionsModification [

	aClassExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitClassModification: aClassModification [

	aClassModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitClassModificationForcingNewVersion: aClassModification [

	aClassModification
		addModificationForcingNewClassVersionToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitExtensionMethodModification: anExtensionMethodModification [

	anExtensionMethodModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitExtensionMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addExtensionModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitExtensionMethodsModification: anExtensionMethodsModification [

	anExtensionMethodsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitMethodModification: aMethodModification [

	aMethodModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitPackageModification: aPackageModification [

	| propertiesModification |
	aPackageModification isAddition
		ifTrue: [ self addAddedPackage: aPackageModification after ].
	aPackageModification isDeletion
		ifTrue: [ self addDeletedPackage: aPackageModification before ].
	currentPackage := aPackageModification after.
	propertiesModification := aPackageModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: [ "https://github.com/dalehenrich/Rowan/issues/180: noop"].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitPackageSetModification: aPackageSetModification [

	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitPackagesModification: aPackagesModification [

	aPackagesModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitProjecteSetModification: aProjectSetModification [

	aProjectSetModification 
		movedPackages do: [:movedPackage | 
			"not a thing right now https://github.com/dalehenrich/Rowan/issues/254" 
			self error: 'internal error - moved packages not yet implemented' ].
	aProjectSetModification movedClasses do: [:movedClass | 
		movedClass addMovedClassToPatchSet: patchSet ].
	aProjectSetModification movedMethods do: [:movedMethod | 
		movedMethod addMovedMethodToPatchSet: patchSet ].
	aProjectSetModification elementsModified
		do: [ :each | each acceptVisitor: self ]

]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitProjectModification: aProjectModification [

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
RwGsImagePatchVisitor_254 >> visitPropertiesModification: aPropertiesModification [

	self error: 'internal error - not yet implemented'

]

{ #category : 'visting' }
RwGsImagePatchVisitor_254 >> visitSourceModification: aSourceModification [

	self error: 'internal error - not yet implemented'

]
