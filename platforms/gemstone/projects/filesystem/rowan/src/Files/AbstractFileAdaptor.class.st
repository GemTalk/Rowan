Class {
	#name : 'AbstractFileAdaptor',
	#superclass : 'Object',
	#instVars : [
		'file'
	],
	#category : 'Files-Core'
}

{ #category : 'accessing' }
AbstractFileAdaptor class >> createDirectory: aPathName [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> currentWorkingDirectoryPath [

	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> deleteDirectory: aPathName [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> deleteFile: aPathName [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> entriesIn: pathString [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> isDirectory: aPathName [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> lookupDirectory: path filename: basename [

	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> lookupDirectoryEntry: fullPath [
	self subclassResponsibility
]

{ #category : 'instance creation' }
AbstractFileAdaptor class >> on: aFile [
	^self new
		file: aFile;
		yourself
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> open: aFileReference withOptions: aFileOptions [
	| aFileAdaptor |
	aFileAdaptor := self openPathName: aFileReference fullName withOptions: aFileOptions.
	aFileAdaptor ifNil: [FileDoesNotExistException signalWith: aFileReference fullName].
	^aFileAdaptor
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> openPathName: aPathname withOptions: aFileOptions [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> rename: oldFileFullName to: newFileFullName [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor >> atEnd: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> close: handle [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFileAdaptor >> file [
	^file
]

{ #category : 'accessing' }
AbstractFileAdaptor >> file: aFile [
	file := aFile
]

{ #category : 'positioning' }
AbstractFileAdaptor >> flush: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> fullName [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> getPosition: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> isClosed: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> read: handle into: readBuffer startingAt: startIndex count: count [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> setPosition: handle to: aPosition [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sizeOf: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sizeOrNil: id [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sync: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> truncate: handle to: anInteger [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> write: handle from: aByteArray startingAt: anIndex count: amount [
	self subclassResponsibility
]
