"
I am an abstract superclass for file handle implementations. I provide a uniform interface that streams can use for read and write operations on a file regardless of the filesystem. I encapsulate the actual IO primitives.
"
Class {
	#name : 'FileSystemHandle',
	#superclass : 'Object',
	#instVars : [
		'reference',
		'writable'
	],
	#category : 'FileSystem-Core-Kernel'
}

{ #category : 'instance creation' }
FileSystemHandle class >> on: aReference writable: aBoolean [
	^ self new setReference: aReference writable: aBoolean
]

{ #category : 'instance creation' }
FileSystemHandle class >> open: aReference writable: aBoolean [
	^ (self on: aReference writable: aBoolean) open
]

{ #category : 'public' }
FileSystemHandle >> at: index [
	| buffer |
	buffer := ByteArray new: 1.
	self at: index read: buffer startingAt: 1 count: 1.
	^ buffer at: 1
]

{ #category : 'public' }
FileSystemHandle >> at: index put: anObject [
	| buffer |
	buffer := ByteArray with: (anObject isCharacter
		ifTrue: [ anObject codePoint ]
		ifFalse: [ anObject ]).
	self at: index write: buffer startingAt: 1 count: 1.
	
]

{ #category : 'public' }
FileSystemHandle >> at: offset read: buffer startingAt: start count: count [
	self subclassResponsibility
]

{ #category : 'public' }
FileSystemHandle >> at: offset write: buffer startingAt: start count: count [
	self subclassResponsibility
]

{ #category : 'private' }
FileSystemHandle >> basicOpen [
	"get the raw stream description from the filesystem's store"
	^ self fileSystem 
		openStreamDescription: reference path
		writable: self fileMode
]

{ #category : 'streams' }
FileSystemHandle >> binaryReadStream [
	
	self subclassResponsibility 
]

{ #category : 'streams' }
FileSystemHandle >> binaryWriteStream [
	
	self subclassResponsibility 
]

{ #category : 'public' }
FileSystemHandle >> close [
	self subclassResponsibility
]

{ #category : 'public' }
FileSystemHandle >> ensureClosed [
	reference exists ifTrue: [self close]
]

{ #category : 'accessing' }
FileSystemHandle >> fileMode [
	^writable
]

{ #category : 'accessing' }
FileSystemHandle >> fileMode: aString [
	writable := aString
]

{ #category : 'accessing' }
FileSystemHandle >> fileSystem [
	^ reference fileSystem 
]

{ #category : 'public' }
FileSystemHandle >> flush [
	self subclassResponsibility
]

{ #category : 'accessing' }
FileSystemHandle >> fullName [
	^ reference fullName
]

{ #category : 'testing' }
FileSystemHandle >> isOpen [
	self subclassResponsibility
]

{ #category : 'testing' }
FileSystemHandle >> isWritable [

	^ self fileMode isWritable
]

{ #category : 'public' }
FileSystemHandle >> open [
	self subclassResponsibility
]

{ #category : 'public' }
FileSystemHandle >> pathString [

	^self reference pathString
]

{ #category : 'accessing' }
FileSystemHandle >> reference [
	^ reference
]

{ #category : 'accessing' }
FileSystemHandle >> reference: aFileReference [
	reference := aFileReference
]

{ #category : 'public' }
FileSystemHandle >> reopen [
	self close.
	self open
]

{ #category : 'initialize-release' }
FileSystemHandle >> setReference: aReference writable: aBoolean [
	self reference: aReference resolve.
	self fileMode: aBoolean
]

{ #category : 'public' }
FileSystemHandle >> sync [
	self subclassResponsibility
]

{ #category : 'public' }
FileSystemHandle >> truncateTo: anInteger [
	self subclassResponsibility
]
