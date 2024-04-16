Class {
	#name : 'RwAbstractProjectSetModificationVisitor',
	#superclass : 'Object',
	#instVars : [
		'currentProjectDefinition',
		'currentPackageDefinition',
		'currentClassDefinition',
		'currentClassExtension'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwAbstractProjectSetModificationVisitor class >> visit: aProjectSetModification [
	^ self new visit: aProjectSetModification
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> addedClass: aClassModification [
	currentClassDefinition := aClassModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> addedClassExtension: aClassExtensionModification [
	currentClassExtension := aClassExtensionModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> addedMethod: aMethodModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> addedMethodExtension: aMethodExtensionModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> addedPackage: aPackageModification [
	currentPackageDefinition := aPackageModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> addedProject: aProjectModification [
	currentProjectDefinition := aProjectModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> changedClass: aClassModification [
	currentClassDefinition := aClassModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> changedClassExtension: aClassExtensionModification [
	currentClassExtension := aClassExtensionModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> changedMethod: aMethodModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> changedMethodExtension: aMethodExtensionModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> changedPackage: aPackageModification [
	currentPackageDefinition := aPackageModification after
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> changedProject: aProjectModification [
	currentProjectDefinition := aProjectModification after
]

{ #category : 'accessing' }
RwAbstractProjectSetModificationVisitor >> currentClassDefinition [
	^ currentClassDefinition
]

{ #category : 'accessing' }
RwAbstractProjectSetModificationVisitor >> currentClassExtension [
	^ currentClassExtension
]

{ #category : 'accessing' }
RwAbstractProjectSetModificationVisitor >> currentPackageDefinition [
	^ currentPackageDefinition
]

{ #category : 'accessing' }
RwAbstractProjectSetModificationVisitor >> currentPackageDefinition: aRwPackageDefinition [
	currentPackageDefinition := aRwPackageDefinition
]

{ #category : 'accessing' }
RwAbstractProjectSetModificationVisitor >> currentProjectDefinition [
	^ currentProjectDefinition
]

{ #category : 'accessing' }
RwAbstractProjectSetModificationVisitor >> currentProjectDefinition: aRwComponentProjectDefinition [
	currentProjectDefinition := aRwComponentProjectDefinition
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> deletedClass: aClassModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> deletedClassExtension: aClassExtensionModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> deletedMethod: aMethodModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> deletedMethodExtension: aMethodExtensionModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> deletedPackage: aPackageModification [
]

{ #category : 'actions' }
RwAbstractProjectSetModificationVisitor >> deletedProject: aProjectModification [
]

{ #category : 'public' }
RwAbstractProjectSetModificationVisitor >> visit: aProjectSetModification [
	aProjectSetModification acceptVisitor: self
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitClassesModification: aClassesModification [
	aClassesModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitClassExtensionModification: aClassExtensionModification [
	aClassExtensionModification isAddition
		ifTrue: [ 
			self addedClassExtension: aClassExtensionModification.
			aClassExtensionModification instanceMethodsModification acceptVisitor: self.
			aClassExtensionModification classMethodsModification acceptVisitor: self.
			^ self ].
	aClassExtensionModification isDeletion
		ifTrue: [ ^ self deletedClassExtension: aClassExtensionModification ].
	self changedClassExtension: aClassExtensionModification.
	aClassExtensionModification instanceMethodsModification acceptVisitor: self.
	aClassExtensionModification classMethodsModification acceptVisitor: self.
	^ self
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitClassExtensionsModification: aClassExtensionsModification [
	aClassExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitClassModification: aClassModification [
	aClassModification isAddition
		ifTrue: [ 
			self addedClass: aClassModification.
			aClassModification instanceMethodsModification acceptVisitor: self.
			aClassModification classMethodsModification acceptVisitor: self.
			^ self ].
	aClassModification isDeletion
		ifTrue: [ ^ self deletedClass: aClassModification ].
	self changedClass: aClassModification.
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self.
	^ self
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitExtensionMethodModification: aMethodExtensionModification [
	aMethodExtensionModification isAddition
		ifTrue: [ ^ self addedMethodExtension: aMethodExtensionModification ].
	aMethodExtensionModification isDeletion
		ifTrue: [ ^ self deletedMethodExtension: aMethodExtensionModification ].
	^ self changedMethodExtension: aMethodExtensionModification
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitExtensionMethodsModification: aMethodExtensionsModification [
	aMethodExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitMethodModification: aMethodModification [
	aMethodModification isAddition
		ifTrue: [ ^ self addedMethod: aMethodModification ].
	aMethodModification isDeletion
		ifTrue: [ ^ self deletedMethod: aMethodModification ].
	^ self changedMethod: aMethodModification
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitMethodsModification: aMethodsModification [
	aMethodsModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitPackageModification: aPackageModification [
	aPackageModification isAddition
		ifTrue: [ 
			self addedPackage: aPackageModification.
			aPackageModification classesModification acceptVisitor: self.
			aPackageModification classExtensionsModification acceptVisitor: self.
			aPackageModification traitsModification acceptVisitor: self.
			^ self ].
	aPackageModification isDeletion
		ifTrue: [ ^ self deletedPackage: aPackageModification ].
	self changedPackage: aPackageModification.
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self.
	aPackageModification traitsModification acceptVisitor: self.
	^ self
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitPackagesModification: aPackagesModification [
	aPackagesModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitProjecteSetModification: aProjectSetModification [
	aProjectSetModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitProjectModification: aProjectModification [
	aProjectModification isAddition
		ifTrue: [ 
			self addedProject: aProjectModification.
			^ aProjectModification packagesModification acceptVisitor: self ].
	aProjectModification isDeletion
		ifTrue: [ ^ self deletedProject: aProjectModification ].
	self changedProject: aProjectModification.
	^ aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitTraitMethodModification: aTraitMethodModification [
	aTraitMethodModification isAddition
		ifTrue: [ ^ self addedMethod: aTraitMethodModification ].
	aTraitMethodModification isDeletion
		ifTrue: [ ^ self deletedMethod: aTraitMethodModification ].
	^ self changedMethod: aTraitMethodModification
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitTraitModification: aTraitModification [
	aTraitModification isAddition
		ifTrue: [ 
			self addedClass: aTraitModification.
			aTraitModification instanceMethodsModification acceptVisitor: self.
			aTraitModification classMethodsModification acceptVisitor: self.
			^ self ].
	aTraitModification isDeletion
		ifTrue: [ ^ self deletedTrait: aTraitModification ].
	self changedTrait: aTraitModification.
	aTraitModification instanceMethodsModification acceptVisitor: self.
	aTraitModification classMethodsModification acceptVisitor: self.
	^ self
]

{ #category : 'visiting' }
RwAbstractProjectSetModificationVisitor >> visitTraitsModification: aTraitsModification [
	true
		ifTrue: [ aTraitsModification elementsModified do: [ :each | each acceptVisitor: self ] ]
]
