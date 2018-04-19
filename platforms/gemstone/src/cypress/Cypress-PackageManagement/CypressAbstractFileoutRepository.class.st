"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractFileoutRepository',
	#superclass : 'CypressAbstractRepository',
	#instVars : [
		'directoryPath'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressAbstractFileoutRepository class >> on: aDirectory [

	^self new
		initializeForDirectory: aDirectory;
		yourself.

]

{ #category : 'accessing' }
CypressAbstractFileoutRepository >> description [

	| desc |
	desc := super description.
	^desc notEmpty
		ifTrue: [desc]
		ifFalse: [self directoryPath]

]

{ #category : 'accessing' }
CypressAbstractFileoutRepository >> directoryPath [

	^directoryPath

]

{ #category : 'initializing - private' }
CypressAbstractFileoutRepository >> directoryPath:  aString [

	directoryPath := aString

]

{ #category : 'initializing - private' }
CypressAbstractFileoutRepository >> ensureDirectoryPathExists [

	self fileUtils ensureDirectoryExists: self directoryPath

]

{ #category : 'accessing - private' }
CypressAbstractFileoutRepository >> fileUtils [

	^CypressFileUtilities current

]

{ #category : 'initializing - private' }
CypressAbstractFileoutRepository >> initializeCreationOn: aUrl alias: aString [

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString

]

{ #category : 'initializing - private' }
CypressAbstractFileoutRepository >> initializeForDirectory: aDirectory [

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.

]

{ #category : 'initializing - private' }
CypressAbstractFileoutRepository >> initializeReaderAndWriterClasses [
  self subclassResponsibility: #'initializeReaderAndWriterClasses'

]

{ #category : 'initializing - private' }
CypressAbstractFileoutRepository >> initializeUrl: aUrl andAlias: aString [

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.

]

{ #category : 'printing' }
CypressAbstractFileoutRepository >> printDetailsOn: aStream [

	self alias notEmpty
		ifTrue: 
			[aStream
				nextPutAll: self alias;
				nextPutAll: ': '].
	aStream nextPutAll: self url printString

]

{ #category : 'validating - private' }
CypressAbstractFileoutRepository >> validateUrl [
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']

]
