"
I provide an abstraction layer to adapt the theoretical file implementation in the FileSystem package with the actual file implemenation in the target Smalltalk dialect.

Each Smalltalk dialect can create its own subclass.

To change which file class to use, update the method FileSystem class>>#fileClass. This should be implemented in dialect dependent package.
"
Class {
	#name : 'AbstractFileAdaptor',
	#superclass : 'Object',
	#instVars : [
		'file'
	],
	#category : 'FileSystem-Core-Kernel'
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

{ #category : 'accessing - file' }
AbstractFileAdaptor >> atEnd [
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

{ #category : 'accessing - file' }
AbstractFileAdaptor >> fileSize [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> flush [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> fullName [
	self subclassResponsibility
]

{ #category : 'accessing - file' }
AbstractFileAdaptor >> isClosed [

	self subclassResponsibility
]

{ #category : 'accessing - file' }
AbstractFileAdaptor >> isWritable [

	self subclassResponsibility
]

{ #category : 'accessing - file' }
AbstractFileAdaptor >> pathString [

	^self file pathString
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sizeOf: handle [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> truncate: anInteger [
	self subclassResponsibility
]

{ #category : 'accessing - file' }
AbstractFileAdaptor >> writeFrom: aByteArray startingAt: anIndex count: count [
	self subclassResponsibility
]
