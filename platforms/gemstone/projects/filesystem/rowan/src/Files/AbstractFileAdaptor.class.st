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
AbstractFileAdaptor class >> openReference: aFileReference withOptions: aFileOptions [
	| aFileAdaptor |
	aFileAdaptor := self openPathName: aFileReference fullName withOptions: aFileOptions.
	aFileAdaptor ifNil: [FileDoesNotExistException signalWith: aFileReference fullName].
	^aFileAdaptor
]

{ #category : 'accessing' }
AbstractFileAdaptor class >> rename: oldFileFullName to: newFileFullName [
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractFileAdaptor >> atEnd: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^self file atEnd
]

{ #category : 'positioning' }
AbstractFileAdaptor >> close: handle [
	"The handle is always nil. This is here for Pharo compatability"

	self file close
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
	"The handle is always nil. This is here for Pharo compatability"

	self file flush
]

{ #category : 'positioning' }
AbstractFileAdaptor >> fullName [
	^self file pathName
]

{ #category : 'positioning' }
AbstractFileAdaptor >> getPosition: handle [
	"The handle is always nil. This is here for Pharo compatability"
		^self file position ifNil: [ self error: 'position error' ]
]

{ #category : 'positioning' }
AbstractFileAdaptor >> isClosed: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^self file isOpen not
]

{ #category : 'positioning' }
AbstractFileAdaptor >> read: handle into: readBuffer startingAt: startIndex count: count [
	"The handle is always nil. This is here for Pharo compatability"

	^(self file read: count into: readBuffer) ifNil: [FileReadError signal: 'File Read Error']
]

{ #category : 'positioning' }
AbstractFileAdaptor >> setPosition: handle to: aPosition [
	"The handle is always nil. This is here for Pharo compatability"
		(self file position: aPosition) ifNil: [ self error: 'position error' ]
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sizeOf: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^(self file fileSize) ifNil: [ self error: 'file size error' ]
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sizeOrNil: id [

	^self file fileSize
]

{ #category : 'positioning' }
AbstractFileAdaptor >> sync: handle [
	"The handle is always nil. This is here for Pharo compatability"

	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing

	self file sync



	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
	self error: 'sync not yet implmented'

	"fsync() failing cannot be ignored"
]

{ #category : 'positioning' }
AbstractFileAdaptor >> truncate: handle to: anInteger [

	self setPosition: handle to: anInteger
	
	"work needs to be done here to call out to the bash shell script 'truncate -s anInteger' "
]

{ #category : 'positioning' }
AbstractFileAdaptor >> write: handle from: aByteArray startingAt: anIndex count: amount [
	"The handle is always nil. This is here for Pharo compatability"

	^(self file nextPutAll: aByteArray) ifNil: [
		(FileWriteError fileName: self fullName) signal: ('File ' , self file name , ' write failed')]
]
