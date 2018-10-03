Class {
	#name : 'RwGsModificationTopazWriterVisitor',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'topazFilename',
		'filenameExtension',
		'currentPackageDefinition',
		'classDefinitions',
		'classExtensions'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedClass: aClassModification [

	self processClass: aClassModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedClassExtension: aClassExtensionModification [

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedPackage: aPackageModification [

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedProject: aProjectModification [

	| addedProject |
	super addedProject: aProjectModification.
	addedProject := aProjectModification after.
	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedClass: aClassModification [

	self processClass: aClassModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedClassExtension: aClassExtensionModification [

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedPackage: aPackageModification [

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedProject: aProjectModification [

	super changedProject: aProjectModification.
self halt.

	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classDefinitions [

	^ classDefinitions ifNil: [ classDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classExtensions [

	^ classExtensions ifNil: [ classExtensions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> currentPackageDefinition [

	^ currentPackageDefinition
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededClass: aClassModification [

	"a deleted class is ignored for topaz ... the deleted class is simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededClassExtension: aClassExtensionModification [

	"a deleted class extension is ignored for topaz ... the deleted methods are simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededPackage: aPackageModification [

	"a deleted package is ignored for topaz ... the deleted classes and methods are simply not written out"

self halt. "if we end up writing out per package topaz files, then something significant needs to happen here"
]

{ #category : 'project writing' }
RwGsModificationTopazWriterVisitor >> ensureProjectDirectoryStructureFor: aProjectDefinition [

	"topaz files do not go in a standard spot in the project directory structure. The files are written directly in the repositoryRootPath"
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> filenameExtension [

	^ filenameExtension ifNil: ['gs' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitor >> processClass: aClassModification [

	| classDefinition |
	classDefinition := aClassModification after.
	(self classDefinitions at: classDefinition name ifAbsent: []) ifNotNil: [ self error: 'duplicate class defintion for ', classDefinition name printString, ' encountered.'].
	self classDefinitions at: classDefinition name put: classDefinition.

	"no need to visit any further as the class definition records the instance and class methods"
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitor >> processClassExtension: aClassExtensionModification [

	| classExtension |
	classExtension := aClassExtensionModification after.
	(self classExtensions at: classExtension name ifAbsent: []) ifNotNil: [ self error: 'duplicate class extension defintion for ', classExtension name printString, ' encountered.'].
	self classExtensions at: classExtension name put: classExtension.

	"no need to visit any further as the class etension records the instance and class methods"
]

{ #category : 'package writing' }
RwGsModificationTopazWriterVisitor >> processPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename [

	^ topazFilename
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename: aString [

	topazFilename := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename: aNameString filenameExtension: anExtensionString [

	topazFilename := aNameString.
	filenameExtension := anExtensionString
]
