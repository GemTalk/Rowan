"
I am an abstract superclass for errors that may occur during filesystem operations.
"
Class {
	#name : 'FileSystemError',
	#superclass : 'Error',
	#instVars : [
		'reference'
	],
	#category : 'FileSystem-Core'
}

{ #category : 'instance creation' }
FileSystemError class >> reference: aReference [
	^ self reference: aReference text: aReference printString
]

{ #category : 'instance creation' }
FileSystemError class >> reference: aReference text: aString [
	^ self basicNew initializeWithReference: aReference text: aString
]

{ #category : 'instance creation' }
FileSystemError class >> signalWith: aReference [
	^ (self reference: aReference) signal
]

{ #category : 'instance creation' }
FileSystemError class >> signalWith: aReference text: aString [
	^ (self reference: aReference text: aString) signal
]

{ #category : 'initialize-release' }
FileSystemError >> initializeWithReference: aReference text: aString [
	reference := aReference.
	messageText := aString
]

{ #category : 'testing' }
FileSystemError >> isResumable [
	^ true
]

{ #category : 'accessing' }
FileSystemError >> reference [
	^ reference
]
