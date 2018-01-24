"
Walks a diff tree and adds patches to a patchSet that will update the GemStone image (repository) to the code state represented by the 'after' state of the diff, provided that the 'before' state of the diff is the current state of the image.
"
Class {
	#name : 'RwGsImagePatchVisitor',
	#superclass : 'Object',
	#instVars : [
		'patchSet',
		'currentPackage',
		'currentClass',
		'currentMethod'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsImagePatchVisitor class >> new [

	^super new initialize
]

{ #category : 'private' }
RwGsImagePatchVisitor >> _patchSetClass [

	^ RwGsPatchSetBasic
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addAddedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addAddedPackage: currentPackage.
	self addClasses: currentPackage classDefinitions.
	self addClassExtensions: currentPackage classExtensions
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addClasses: classDefinitions [

	classDefinitions do: 
			[:classDefinition |
			patchSet addAddedClass: classDefinition inPackage: currentPackage.
			classDefinition classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addAddedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage].
			classDefinition instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addAddedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage]]
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addClassExtensions: classExtensions [

	classExtensions do: 
			[:classDefinition |
			patchSet addExtendedClass: classDefinition inPackage: currentPackage.
			classDefinition classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addExtendedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage].
			classDefinition instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addExtendedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage]]
]

{ #category : 'private' }
RwGsImagePatchVisitor >> addPackages: packageDefinitions [

	packageDefinitions
		do: [:packageDefinition | self addAddedpackage: packageDefinition]
]

{ #category : 'private' }
RwGsImagePatchVisitor >> deleteClasses: classDefinitions [

	classDefinitions do: 
			[:classDefinition |
			patchSet addDeletedClass: classDefinition inPackage: currentPackage.
			classDefinition classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addDeletedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage].
			classDefinition instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addDeletedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage]]
]

{ #category : 'private' }
RwGsImagePatchVisitor >> deletePackages: packageDefinitions [

	packageDefinitions do: 
			[:packageDefinition |
			currentPackage := packageDefinition.
			patchSet addDeletedPackage: currentPackage.
			self deleteClasses: packageDefinition classDefinitions.
			packageDefinition classExtensions do: 
					[:classExtension |
					classExtension classMethodDefinitions do: 
							[:classMethodDefinition |
							patchSet
								addDeletedClassMethod: classMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition].
					classExtension instanceMethodDefinitions do: 
							[:instanceMethodDefinition |
							patchSet
								addDeletedInstanceMethod: instanceMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition]]]
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
RwGsImagePatchVisitor >> visitClassExtensionsModification: aClassModification [

	aClassModification isEmpty
		ifFalse: [ self halt ]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitClassModification: aClassModification [

	aClassModification 
		addModificationToPatchSet: patchSet 
		inPackage: currentPackage.
	currentClass := aClassModification after.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self

]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitMethodModification: aMethodModification [

	aMethodModification 
		addModificationToPatchSet: patchSet 
		inPackage: currentPackage.

]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitMethodModificationForNewClassVersion: aMethodModification [

	aMethodModification
		addModificationToPatchSetForNewClassVersion: patchSet
		inPackage: currentPackage
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPackageModification: aPackageModification [

	| propertiesModification classesModification classExtensionsModification |
	aPackageModification isAddition
		ifTrue: 
			[self addAddedPackage: aPackageModification after.
			^self].
	aPackageModification isDeletion
		ifTrue: 
			[self addDeletedPackage: aPackageModification before.
			^self].
	currentPackage := aPackageModification after.
	propertiesModification := aPackageModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: 
			[self halt: 'What do we do with this patch we''re creating here?'.
			RwGsPackagePropertiesPatch forPackageProperties: propertiesModification
				inPackage: currentPackage].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPackageSetModification: aPackageSetModification [

	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitPropertiesModification: aPropertiesModification [

	self halt
]

{ #category : 'visting' }
RwGsImagePatchVisitor >> visitSourceModification: aSourceModification [

	self halt
]
