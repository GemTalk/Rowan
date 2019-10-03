Class {
	#name : 'RwSample1TestValidationVisitor',
	#superclass : 'RwAbstractProjectSetModificationVisitor',
	#instVars : [
		'defaultNoopBlock',
		'addedClassBlock',
		'addedClassExtensionBlock',
		'addedMethodBlock',
		'addedMethodExtensionBlock',
		'addedPackageBlock',
		'addedProjectBlock',
		'changedClassBlock',
		'changedClassExtensionBlock',
		'changedMethodBlock',
		'changedMethodExtensionBlock',
		'changedPackageBlock',
		'changedProjectBlock',
		'deletedClassBlock',
		'deletedClassExtensionBlock',
		'deletedMethodBlock',
		'deletedMethodExtensionBlock',
		'deletedPackageBlock',
		'deletedProjectBlock'
	],
	#category : 'Rowan-Tests-35x'
}

{ #category : 'actions' }
RwSample1TestValidationVisitor >> addedClass: aClassModification [
	currentClassDefinition := aClassModification after.
	self addedClassBlock cull: aClassModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedClassBlock [
	^ addedClassBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedClassBlock: aBlock [
	addedClassBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> addedClassExtension: aClassExtensionModification [
	currentClassExtension := aClassExtensionModification after.
	self addedClassExtensionBlock cull: aClassExtensionModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedClassExtensionBlock [
	^ addedClassExtensionBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedClassExtensionBlock: aBlock [
	addedClassExtensionBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> addedMethod: aMethodModification [
	self addedMethodBlock cull: aMethodModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedMethodBlock [
	^ addedMethodBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedMethodBlock: aBlock [
	addedMethodBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> addedMethodExtension: aMethodExtensionModification [
	self addedMethodExtensionBlock cull: aMethodExtensionModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedMethodExtensionBlock [
	^ addedMethodExtensionBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedMethodExtensionBlock: aBlock [
	addedMethodExtensionBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> addedPackage: aPackageModification [
	currentPackageDefinition := aPackageModification after.
	self addedPackageBlock cull: aPackageModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedPackageBlock [
	^ addedPackageBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedPackageBlock: aBlock [
	addedPackageBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> addedProject: aProjectModification [
	currentProjectDefinition := aProjectModification after.
	self addedProjectBlock cull: aProjectModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedProjectBlock [
	^ addedProjectBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> addedProjectBlock: aBlock [
	addedProjectBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> changedClass: aClassModification [
	currentClassDefinition := aClassModification after.
	self changedClassBlock cull: aClassModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedClassBlock [
	^ changedClassBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedClassBlock: aBlock [
	changedClassBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> changedClassExtension: aClassExtensionModification [
	currentClassExtension := aClassExtensionModification after.
	self changedClassExtensionBlock cull: aClassExtensionModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedClassExtensionBlock [
	^ changedClassExtensionBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedClassExtensionBlock: aBlock [
	changedClassExtensionBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> changedMethod: aMethodModification [
	self changedMethodBlock cull: aMethodModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedMethodBlock [
	^ changedMethodBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedMethodBlock: aBlock [
	changedMethodBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> changedMethodExtension: aMethodExtensionModification [
	self changedMethodExtensionBlock cull: aMethodExtensionModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedMethodExtensionBlock [
	^ changedMethodExtensionBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedMethodExtensionBlock: aBlock [
	changedMethodExtensionBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> changedPackage: aPackageModification [
	currentPackageDefinition := aPackageModification after.
	self changedPackageBlock cull: aPackageModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedPackageBlock [
	^ changedPackageBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedPackageBlock: aBlock [
	changedPackageBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> changedProject: aProjectModification [
	currentProjectDefinition := aProjectModification after.
	self changedProjectBlock cull: aProjectModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedProjectBlock [
	^ changedProjectBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> changedProjectBlock: aBlock [
	changedProjectBlock := aBlock
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> defaultNoopBlock [
	^ defaultNoopBlock ifNil: [ [  ] ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> defaultNoopBlock: aBlock [
	defaultNoopBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> deletedClass: aClassModification [
	self deletedClassBlock cull: aClassModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedClassBlock [
	^ deletedClassBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedClassBlock: aBlock [
	deletedClassBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> deletedClassExtension: aClassExtensionModification [
	self deletedClassExtensionBlock cull: aClassExtensionModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedClassExtensionBlock [
	^ deletedClassExtensionBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedClassExtensionBlock: aBlock [
	deletedClassExtensionBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> deletedMethod: aMethodModification [
	self deletedMethodBlock cull: aMethodModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedMethodBlock [
	^ deletedMethodBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedMethodBlock: aBlock [
	deletedMethodBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> deletedMethodExtension: aMethodExtensionModification [
	self deletedMethodExtensionBlock cull: aMethodExtensionModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedMethodExtensionBlock [
	^ deletedMethodExtensionBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedMethodExtensionBlock: aBlock [
	deletedMethodExtensionBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> deletedPackage: aPackageModification [
	self deletedPackageBlock cull: aPackageModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedPackageBlock [
	^ deletedPackageBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedPackageBlock: aBlock [
	deletedPackageBlock := aBlock
]

{ #category : 'actions' }
RwSample1TestValidationVisitor >> deletedProject: aProjectModification [
	self deletedProjectBlock cull: aProjectModification cull: self
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedProjectBlock [
	^ deletedProjectBlock ifNil: [ self defaultNoopBlock ]
]

{ #category : 'accessing' }
RwSample1TestValidationVisitor >> deletedProjectBlock: aBlock [
	deletedProjectBlock := aBlock
]
