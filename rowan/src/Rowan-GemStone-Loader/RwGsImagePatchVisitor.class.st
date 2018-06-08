"
Walks a diff tree and adds patches to a patchSet that will update the GemStone image (repository) to the code state represented by the 'after' state of the diff, provided that the 'before' state of the diff is the current state of the image.
"
Class {
	#name : 'RwGsImagePatchVisitor',
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
RwGsImagePatchVisitor class >> new [

	^super new initialize
]

{ #category : 'private' }
RwGsImagePatchVisitor >> _patchSetClass [

	^ RwGsPatchSet
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addAddedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addAddedPackage: currentPackage inProject: currentProject.
	self addClasses: currentPackage classDefinitions.
	self addClassExtensions: currentPackage classExtensions

]

{ #category : 'private' }
RwGsImagePatchVisitor >> addAddedProject: aProjectDefinition [

	currentProject := aProjectDefinition.
	patchSet addAddedProject: currentProject
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addClasses: classDefinitions [

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
RwGsImagePatchVisitor >> addClassExtensions: classExtensions [

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
RwGsImagePatchVisitor >> addDeletedPackage: aPackageDefinition [

	self deletePackages: {aPackageDefinition}
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addPackages: packageDefinitions [

	packageDefinitions
		do: [:packageDefinition | self addAddedpackage: packageDefinition]
]

{ #category : 'private' }
RwGsImagePatchVisitor >> deleteClasses: classDefinitions [

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

{ #category : 'private' }
RwGsImagePatchVisitor >> deletePackages: packageDefinitions [

	packageDefinitions
		do: [ :packageDefinition | 
			currentPackage := packageDefinition.
			patchSet addDeletedPackage: currentPackage inProject: currentProject.
			self deleteClasses: packageDefinition classDefinitions.
			packageDefinition classExtensions
				do: [ :classExtension | 
					classExtension classMethodDefinitions
						do: [ :classMethodDefinition | 
							patchSet
								addDeletedClassMethod: classMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition
								inProject: currentProject ].
					classExtension instanceMethodDefinitions
						do: [ :instanceMethodDefinition | 
							patchSet
								addDeletedInstanceMethod: instanceMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition
								inProject: currentProject ] ] ]
]

{ #category : 'initialization' }
RwGsImagePatchVisitor >> initialize [

	patchSet := self _patchSetClass new
]

{ #category : 'accessing' }
RwGsImagePatchVisitor >> patchSet [
	^patchSet

]

{ #category : 'public' }
RwGsImagePatchVisitor >> visit: aPackageSetModification [

	^aPackageSetModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitClassesModification: aClassesModification [

	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitClassExtensionModification: aClassExtensionModification [

	aClassExtensionModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassExtensionModification after.
	aClassExtensionModification instanceMethodsModification acceptVisitor: self.
	aClassExtensionModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitClassExtensionsModification: aClassExtensionsModification [

	aClassExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitClassModification: aClassModification [

	aClassModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitClassModificationForcingNewVersion: aClassModification [

	aClassModification
		addModificationForcingNewClassVersionToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitExtensionMethodModification: anExtensionMethodModification [

	anExtensionMethodModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitExtensionMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addExtensionModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitExtensionMethodsModification: anExtensionMethodsModification [

	anExtensionMethodsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitMethodModification: aMethodModification [

	aMethodModification
		addModificationToPatchSet: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
		inProject: currentProject
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPackageModification: aPackageModification [

	| propertiesModification |
	aPackageModification isAddition
		ifTrue: [ 
			self addAddedPackage: aPackageModification after.
			^ self ].
	aPackageModification isDeletion
		ifTrue: [ 
			self addDeletedPackage: aPackageModification before.
			^ self ].
	currentPackage := aPackageModification after.
	propertiesModification := aPackageModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: [ 
			"https://github.com/dalehenrich/Rowan/issues/180"
			self error: 'internal error - unexpected non-empty package properties modification. See Issue #180' ].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPackageSetModification: aPackageSetModification [

	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPackagesModification: aPackagesModification [

	aPackagesModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitProjecteSetModification: aProjectSetModification [

	aProjectSetModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitProjectModification: aProjectModification [

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
			self error: 'internal error - unexpected non-empty project properties modification. See Issue #180'  ].
	aProjectModification packagesModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPropertiesModification: aPropertiesModification [

	self error: 'internal error - not yet implemented'

]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitSourceModification: aSourceModification [

	self error: 'internal error - not yet implemented'

]
