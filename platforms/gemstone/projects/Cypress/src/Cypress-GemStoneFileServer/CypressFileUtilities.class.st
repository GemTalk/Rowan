"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileUtilities',
	#superclass : 'Object',
	#classVars : [
		'Current'
	],
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'accessing' }
CypressFileUtilities class >> current [

	^Current

]

{ #category : 'utilities' }
CypressFileUtilities class >> deleteAll: aDirectory [

	self subclassResponsibility: #deleteAll:

]

{ #category : 'utilities' }
CypressFileUtilities class >> deleteAll: aDirectory rejecting: rejectBlock [
  self subclassResponsibility: #'deleteAll:rejecting:'

]

{ #category : 'utilities' }
CypressFileUtilities class >> directoryEntriesFrom: aDirectory [

	self subclassResponsibility: #directoryEntriesFrom:

]

{ #category : 'utilities' }
CypressFileUtilities class >> directoryExists: aDirectory [

	self subclassResponsibility: #directoryExists:

]

{ #category : 'unknown' }
CypressFileUtilities class >> directoryFileNamesAndContents: aDirectory [

	self subclassResponsibility: #directoryFileNamesAndContents:

]

{ #category : 'utilities' }
CypressFileUtilities class >> directoryFromPath: directoryPath relativeTo: aDirectory [

	self subclassResponsibility: #directoryFromPath:relativeTo:

]

{ #category : 'utilities' }
CypressFileUtilities class >> ensureDirectoryExists: aDirectory [

	self subclassResponsibility: #ensureDirectoryExists:

]

{ #category : 'initializating' }
CypressFileUtilities class >> install [

	Current := self

]

{ #category : 'utilities' }
CypressFileUtilities class >> localNameFrom: aDirectory [

	self subclassResponsibility: #localNameFrom:

]

{ #category : 'utilities' }
CypressFileUtilities class >> pathNameDelimiter [

	self subclassResponsibility: #pathNameDelimiter

]

{ #category : 'utilities' }
CypressFileUtilities class >> readStreamFor: filePath do: aOneArgBlock [

	self subclassResponsibility: #readStreamFor:do:

]

{ #category : 'utilities' }
CypressFileUtilities class >> readStreamFor: filePath in: aDirectory do: aOneArgBlock [

	self subclassResponsibility: #readStreamFor:in:do:

]

{ #category : 'utilities' }
CypressFileUtilities class >> workingDirectory [

	self subclassResponsibility: #workingDirectory

]

{ #category : 'utilities' }
CypressFileUtilities class >> writeStreamFor: filePath in: aDirectory do: aOneArgBlock [

	self subclassResponsibility: #writeStreamFor:in:do:

]
