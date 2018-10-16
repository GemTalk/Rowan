"
Abstract class with support for walking a modification tree and writing out the source code in a variety of formats:
	topaz fileout
	Tonel
	Filetree
"
Class {
	#name : 'RwModificationWriterVisitor',
	#superclass : 'Object',
	#instVars : [
		'currentProjectDefinition',
		'currentPackageDefinition',
		'repositoryRootPath'
	],
	#category : 'Rowan-Core'
}

{ #category : 'actions' }
RwModificationWriterVisitor >> addedClass: aClassModification [

	self processClass: aClassModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> addedClassExtension: aClassExtensionModification [

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> addedPackage: aPackageModification [

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> addedProject: aProjectModification [
	| addedProjectDefinition |
	addedProjectDefinition := aProjectModification after.
	self repositoryRootPath ifNil: [ self repositoryRootPath: addedProjectDefinition repositoryRootPath ].
	self ensureProjectDirectoryStructureFor: addedProjectDefinition.
	self processProject: aProjectModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> changedClass: aClassModification [

	self processClass: aClassModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> changedClassExtension: aClassExtensionModification [

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> changedPackage: aPackageModification [

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwModificationWriterVisitor >> changedProject: aProjectModification [
	| afterProject |
	afterProject := aProjectModification after.
	self repositoryRootPath ifNil: [ self repositoryRootPath: afterProject repositoryRootPath ].
self halt.
]

{ #category : 'accessing' }
RwModificationWriterVisitor >> currentPackageDefinition [

	^ currentPackageDefinition
]

{ #category : 'accessing' }
RwModificationWriterVisitor >> currentProjectDefinition [

	^ currentProjectDefinition
]

{ #category : 'actions' }
RwModificationWriterVisitor >> deletedClass: aClassModification [

	self subclassResponsibility: #deletedClass:
]

{ #category : 'actions' }
RwModificationWriterVisitor >> deletedClassExtension: aClassExtensionModification [

	self subclassResponsibility: #deletedClassExtension:
]

{ #category : 'actions' }
RwModificationWriterVisitor >> deletedPackage: aPackageModification [

	self subclassResponsibility: #deletedPackage:
]

{ #category : 'actions' }
RwModificationWriterVisitor >> deletedProject: aProjectModification [

	"A deleted project means that the project was not inlcuded in the base project set definition ... we do not want to change anything on disk.
		So ignore the rest of the project"
]

{ #category : 'project writing' }
RwModificationWriterVisitor >> ensureProjectDirectoryStructureFor: aProjectDefinition [

	(self repositoryRootPath / aProjectDefinition repoPath) ensureCreateDirectory
]

{ #category : 'exporting' }
RwModificationWriterVisitor >> export [

	self subclassResponsibility: #export
]

{ #category : 'class writing' }
RwModificationWriterVisitor >> processClass: aClassModification [

	self subclassResponsibility: #processClass:
]

{ #category : 'class writing' }
RwModificationWriterVisitor >> processClassExtension: aClassExtensionModification [

	self subclassResponsibility: #processClassExtension:
]

{ #category : 'package writing' }
RwModificationWriterVisitor >> processPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self
]

{ #category : 'project writing' }
RwModificationWriterVisitor >> processProject: aProjectModification [

	| x |
	currentProjectDefinition := aProjectModification after.
	x := STON fromStream: (ZnBufferedReadStream on: (self repositoryRootPath / currentProjectDefinition repoPath / 'properties.st') readStream).
	(x at: #format) = 'tonel' ifFalse: [ self error: 'expected tonel format repository' ].
	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'accessing' }
RwModificationWriterVisitor >> repositoryRootPath [

	^ repositoryRootPath
]

{ #category : 'accessing' }
RwModificationWriterVisitor >> repositoryRootPath: aStringOrFileReference [

	repositoryRootPath := aStringOrFileReference asFileReference.
	repositoryRootPath exists ifFalse: [ self error: 'The repository root path must exist: ', repositoryRootPath pathString printString ]
]

{ #category : 'public' }
RwModificationWriterVisitor >> visit: aPackageSetModification [

	^aPackageSetModification acceptVisitor: self
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitClassesModification: aClassesModification [

	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitClassExtensionModification: aClassExtensionModification [

	aClassExtensionModification isAddition
		ifTrue: [ ^ self addedClassExtension: aClassExtensionModification ] .
	aClassExtensionModification isDeletion
		ifTrue: [ ^ self deletedClassExtension: aClassExtensionModification ].
	^ self changedClassExtension: aClassExtensionModification
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitClassExtensionsModification: aClassExtensionsModification [

	aClassExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitClassModification: aClassModification [

	aClassModification isAddition
		ifTrue: [ ^ self addedClass: aClassModification ] .
	aClassModification isDeletion
		ifTrue: [ ^ self deletedClass: aClassModification ].
	^ self changedClass: aClassModification
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitPackageModification: aPackageModification [

	aPackageModification isAddition
		ifTrue: [ ^ self addedPackage: aPackageModification ] .
	aPackageModification isDeletion
		ifTrue: [ ^ self deletedPackage: aPackageModification ].
	^ self changedPackage: aPackageModification
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitPackagesModification: aPackagesModification [

	aPackagesModification elementsModified do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitProjecteSetModification: aProjectSetModification [

	aProjectSetModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwModificationWriterVisitor >> visitProjectModification: aProjectModification [

	| propertiesModification |
	aProjectModification isAddition
		ifTrue: [ ^ self addedProject: aProjectModification ].
	aProjectModification isDeletion
		ifTrue: [ ^ self deletedProject: aProjectModification ].
	^ self changedProject: aProjectModification
]
