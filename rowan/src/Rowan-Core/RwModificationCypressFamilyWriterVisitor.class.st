Class {
	#name : 'RwModificationCypressFamilyWriterVisitor',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'instanceFileNameMap',
		'classFileNameMap',
		'classDefFileNameMap',
		'classExtFileNameMap',
		'packageDefFileNameMap',
		'packageDefBeforeFileNameMap'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwModificationCypressFamilyWriterVisitor class >> orderedDictionaryClass [
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary
]

{ #category : 'class writing' }
RwModificationCypressFamilyWriterVisitor >> _commentOf: aClassDefinition [
	^ (aClassDefinition comment 
		copyReplaceAll: '"' 
		with: '""')
		withLineEndings: self _newLine
]

{ #category : 'package writing' }
RwModificationCypressFamilyWriterVisitor >> _packageSourceDir [

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
RwModificationCypressFamilyWriterVisitor >> _projectSourceDir [

	^ self currentProjectDefinition packagesRoot
]

{ #category : 'private' }
RwModificationCypressFamilyWriterVisitor >> _toJSON: anObject [
	^ (STON toJsonStringPretty: anObject)
		withLineEndings: self _newLine
]

{ #category : 'private' }
RwModificationCypressFamilyWriterVisitor >> _toSTON: anObject [
	^ (String streamContents: [ :stream | 
		(TonelSTONWriter on: stream) nextPut: anObject ])
		withLineEndings: self _newLine
]

{ #category : 'actions' }
RwModificationCypressFamilyWriterVisitor >> changedPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.

	self halt. "anything need to be done for tonel?"

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwModificationCypressFamilyWriterVisitor >> deletedPackage: aPackageModification [

	currentPackageDefinition := aPackageModification before.

	self _packageSourceDir ensureDeleteAll.	"delete the whole package directory"
]

{ #category : 'package writing' }
RwModificationCypressFamilyWriterVisitor >> packageExtension [

	^ ''
]

{ #category : 'package writing' }
RwModificationCypressFamilyWriterVisitor >> processPackage: aPackageModification [

	classDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after classDefinitions.
	classExtFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after classExtensions.

	super processPackage: aPackageModification
]
