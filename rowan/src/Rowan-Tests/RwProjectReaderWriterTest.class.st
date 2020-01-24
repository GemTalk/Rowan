Class {
	#name : 'RwProjectReaderWriterTest',
	#superclass : 'RwBrowserToolTest',
	#instVars : [
		'filesystem',
		'toDelete',
		'projectsToDelete'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'testing' }
RwProjectReaderWriterTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'RwProjectReaderWriterTest'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _createFileSystem [
	^ FileSystem disk "currentMemoryFileSystem"
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _markForCleanup: aFileReference [
	toDelete add: aFileReference
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _markForProjectCleanup: aProjectName [
	projectsToDelete add: aProjectName
]

{ #category : 'running' }
RwProjectReaderWriterTest >> setUp [
	super setUp.
	filesystem := self _createFileSystem.
	toDelete := OrderedCollection new.
	projectsToDelete := OrderedCollection new.
]

{ #category : 'running' }
RwProjectReaderWriterTest >> tearDown [
	toDelete
		select: [ :aFileRef | aFileRef exists ]
		thenDo: [ :aFileRef | aFileRef deleteAll ].
	projectsToDelete
		do: [ :projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].
	super tearDown
]
