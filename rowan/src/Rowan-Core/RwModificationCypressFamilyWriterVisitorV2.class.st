Class {
	#name : 'RwModificationCypressFamilyWriterVisitorV2',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'classDefFileNameMap',
		'classExtFileNameMap',
		'traitDefFileNameMap',
		'traitDefBeforeFileNameMap',
		'classDefBeforeFileNameMap',
		'classExtBeforeFileNameMap',
		'packageDefFileNameMap',
		'packageDefBeforeFileNameMap',
		'packagesRoot'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwModificationCypressFamilyWriterVisitorV2 class >> orderedDictionaryClass [
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary
]

{ #category : 'class writing' }
RwModificationCypressFamilyWriterVisitorV2 >> _commentOf: aClassDefinition [
	^ aClassDefinition comment withLineEndings: self _newLine
]

{ #category : 'package writing' }
RwModificationCypressFamilyWriterVisitorV2 >> _packageSourceDir [

	| filename |
	filename := packageDefFileNameMap 
		at:  self currentPackageDefinition name 
		ifAbsent: [
			"in the case of package removal, need to use the before package map"
			packageDefBeforeFileNameMap at: self currentPackageDefinition name ].
	^ self packageExtension isEmpty
		ifTrue: [ self _projectSourceDir /filename ]
		ifFalse: [ self _projectSourceDir /filename, self packageExtension ]
]

{ #category : 'project writing' }
RwModificationCypressFamilyWriterVisitorV2 >> _projectSourceDir [
	^ self packagesRoot
]

{ #category : 'private' }
RwModificationCypressFamilyWriterVisitorV2 >> _toJSON: anObject [
	^ (STON toJsonStringPretty: anObject)
		withLineEndings: self _newLine
]

{ #category : 'private' }
RwModificationCypressFamilyWriterVisitorV2 >> _toSTON: anObject [
	^ (String streamContents: [ :stream | 
		(TonelSTONWriter on: stream) nextPut: anObject ])
		withLineEndings: self _newLine
]

{ #category : 'actions' }
RwModificationCypressFamilyWriterVisitorV2 >> deletedPackage: aPackageModification [

	currentPackageDefinition := aPackageModification before.

	self _packageSourceDir ensureDeleteAll.	"delete the whole package directory"
]

{ #category : 'package writing' }
RwModificationCypressFamilyWriterVisitorV2 >> packageExtension [

	^ ''
]

{ #category : 'accessing' }
RwModificationCypressFamilyWriterVisitorV2 >> packagesRoot [
	^ packagesRoot
]

{ #category : 'accessing' }
RwModificationCypressFamilyWriterVisitorV2 >> packagesRoot: aFileReference [
	packagesRoot := aFileReference
]

{ #category : 'package writing' }
RwModificationCypressFamilyWriterVisitorV2 >> processPackage: aPackageModification [

	classDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after classDefinitions.
	classExtFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after classExtensions.
	traitDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after traitDefinitions.
	classDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification before classDefinitions.
	classExtBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification before classExtensions.
	traitDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification before traitDefinitions.

	super processPackage: aPackageModification
]
