"
Walks a diff tree and adds patches to a patchSet that will update the GemStone image (repository) to the code state represented by the 'after' state of the diff, provided that the 'before' state of the diff is the current state of the image.
"
Class {
	#name : 'CypGsImagePatchVisitor',
	#superclass : 'Object',
	#instVars : [
		'patchSet',
		'currentPackage',
		'currentClass',
		'currentMethod'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsImagePatchVisitor class >> new [

	^super new initialize
]

{ #category : 'private' }
CypGsImagePatchVisitor >> addAddedPackage: aPackageDefinition [

	currentPackage := aPackageDefinition.
	patchSet addAddedPackage: currentPackage.
	self addClasses: currentPackage classDefinitions.
	aPackageDefinition classExtensions do: 
			[:classExtension |
			classExtension classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addAddedClassMethod: classMethodDefinition
						inClass: classExtension
						inPackage: aPackageDefinition].
			classExtension instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addAddedInstanceMethod: instanceMethodDefinition
						inClass: classExtension
						inPackage: aPackageDefinition]]
]

{ #category : 'private' }
CypGsImagePatchVisitor >> addClasses: classDefinitions [

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
CypGsImagePatchVisitor >> addPackages: packageDefinitions [

	packageDefinitions
		do: [:packageDefinition | self addAddedpackage: packageDefinition]
]

{ #category : 'private' }
CypGsImagePatchVisitor >> deleteClasses: classDefinitions [

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
CypGsImagePatchVisitor >> deletePackages: packageDefinitions [

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
CypGsImagePatchVisitor >> initialize [

	patchSet := CypGsPatchSet new
]

{ #category : 'accessing' }
CypGsImagePatchVisitor >> patchSet [
	^patchSet

]

{ #category : 'public' }
CypGsImagePatchVisitor >> visit: aPackageSetModification [

	^aPackageSetModification acceptVisitor: self
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitClassesModification: aClassesModification [

	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitClassExtensionsModification: aClassModification [

	self halt
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitClassModification: aClassModification [

	| propertiesModification classesModification classExtensionsModification |
	aClassModification isAddition
		ifTrue: 
			[patchSet addAddedClass: aClassModification after inPackage: currentPackage.
			"^self"].
	aClassModification isDeletion
		ifTrue: 
			[patchSet addDeletedClass: aClassModification before
				inPackage: currentPackage.
			"^self"].
	currentClass := aClassModification after.
	propertiesModification := aClassModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: [self halt: 'Need to do something with modified properties'].
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitMethodModification: aMethodModification [

	self halt
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitPackageModification: aPackageModification [

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
			CypGsPackagePropertiesPatch forPackageProperties: propertiesModification
				inPackage: currentPackage].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitPackageSetModification: aPackageSetModification [

	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitPropertiesModification: aPropertiesModification [

	self halt
]

{ #category : 'visting' }
CypGsImagePatchVisitor >> visitSourceModification: aSourceModification [

	self halt
]
