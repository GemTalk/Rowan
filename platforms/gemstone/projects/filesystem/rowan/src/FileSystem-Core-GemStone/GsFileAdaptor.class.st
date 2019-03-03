"
I provide an abstraction layer to adapt the theoretical file implementation in the FileSystem package with the actual file implemenation in the target Smalltalk dialect.
"
Class {
	#name : 'GsFileAdaptor',
	#superclass : 'AbstractFileAdaptor',
	#instVars : [
		'options'
	],
	#category : 'FileSystem-Core-GemStone'
}

{ #category : 'accessing - file' }
GsFileAdaptor class >> _entriesIn: pathString [
	| result |
	result := GsFile _contentsOfServerDirectory: pathString expandPath: true.
	^result _isArray ifTrue: [result] ifFalse:  [nil]
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> _fullPath: path filename: basename [
	"If this is a directory, then the path needs to have a slash on the end"
	| fullPath |
	fullPath := path , '/' , basename.
	(self isDirectory: fullPath) == true ifTrue: [^fullPath , '/'].
	^fullPath
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> createDirectory: aPathName [
	^GsFile createServerDirectory: aPathName
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> currentWorkingDirectoryPath [

	^GsFile _expandEnvVariable: 'PWD' isClient:false
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> deleteDirectory: aPathName [
	^GsFile removeServerDirectory: aPathName
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> deleteFile: aPathName [
	^GsFile removeServerFile: aPathName
]

{ #category : 'error handling' }
GsFileAdaptor class >> doesNotUnderstand: aMessage [
	"Redirect message to the resolved version of this GsFile."
	(GsFile respondsTo: aMessage selector)
		ifTrue: [ ^ GsFile perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> entriesIn: pathString [
	| result |
	result := self _entriesIn: pathString.
	^(result ifNil: [#()])
		reject: [:ea | (ea  endsWith: '.') or: [ea endsWith: '..']]
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> isDirectory: aPathName [
	^GsFile _isDirectory: aPathName onClient: false
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> lookupDirectory: path filename: basename [

	| fullPath|
	fullPath := self _fullPath: path filename: basename.
	^self lookupDirectoryEntry: fullPath
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> lookupDirectoryEntry: fullPath [
	| gsFileStat |
	gsFileStat := GsFile stat: fullPath isLstat: true.
	gsFileStat _isSmallInteger ifTrue: [ ^ nil ].
	^	{
		(Path from: fullPath) basename.
		gsFileStat ctimeUtcSeconds .
		gsFileStat mtimeUtcSeconds .
		gsFileStat isDirectory .
		gsFileStat size.
		gsFileStat mode bitAnd: 8r777. "https://github.com/GemTalk/FileSystemGs/issues/11"
		(gsFileStat mode bitAnd: 8r0120000) = 8r0120000. "https://github.com/GemTalk/FileSystemGs/issues/10"
	}
]

{ #category : 'instance creation' }
GsFileAdaptor class >> openPathName: aPathName withOptions: aFileOptions [
	"This is the current way of openning a file"
	| aGsFile |
	aGsFile := 
		GsFile 
			open: aPathName 
			mode: aFileOptions mode 
			onClient: aFileOptions isForeignFileSystem 
			withCompression: aFileOptions isGzipped.
	^aGsFile ifNotNil: [(self on: aGsFile) options: aFileOptions]
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> rename: oldFileFullName to: newFileFullName [
	^(GsFile renameFileOnServer: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]
]

{ #category : 'accessing - file' }
GsFileAdaptor class >> serverErrorString [
	^GsFile serverErrorString
]

{ #category : 'instance creation' }
GsFileAdaptor class >> stderr [

	| aGsFile |
	aGsFile := GsFile _getStdFile: 2 onClient: false.
	^aGsFile ifNotNil: [(self on: aGsFile) options: FileOptions write]
]

{ #category : 'instance creation' }
GsFileAdaptor class >> stdin [

	| aGsFile |
	aGsFile := GsFile _getStdFile: 0 onClient: false.
	^aGsFile ifNotNil: [(self on: aGsFile) options: FileOptions read]
]

{ #category : 'instance creation' }
GsFileAdaptor class >> stdout [

	| aGsFile |
	aGsFile := GsFile _getStdFile: 1 onClient: false.
	^aGsFile ifNotNil: [(self on: aGsFile) options: FileOptions write]
]

{ #category : 'accessing - file' }
GsFileAdaptor >> atEnd [
	^self file atEnd
]

{ #category : 'accessing - file' }
GsFileAdaptor >> close [
	self file close
]

{ #category : 'error handling' }
GsFileAdaptor >> doesNotUnderstand: aMessage [
	"Redirect message to the resolved version of this GsFile."
	(self file respondsTo: aMessage selector)
		ifTrue: [ ^ self file perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
]

{ #category : 'accessing - file' }
GsFileAdaptor >> fileSize [
	^self file fileSize
]

{ #category : 'accessing - file' }
GsFileAdaptor >> flush [
	self file flush
]

{ #category : 'accessing - file' }
GsFileAdaptor >> fullName [
	^self file pathName
]

{ #category : 'accessing - file' }
GsFileAdaptor >> isClosed [
	^self isOpen not
]

{ #category : 'accessing - file' }
GsFileAdaptor >> isOpen [
	^self file notNil and: [self file isOpen]
]

{ #category : 'testing' }
GsFileAdaptor >> isWritable [
	^self options isWritable
]

{ #category : 'accessing - file' }
GsFileAdaptor >> next: count into: buf [
	^self file next: count into: buf
]

{ #category : 'accessing - file' }
GsFileAdaptor >> nextPutAll: stringOrByteArray [
	^self file nextPutAll: stringOrByteArray
]

{ #category : 'accessing' }
GsFileAdaptor >> options [
	^options
]

{ #category : 'accessing' }
GsFileAdaptor >> options: aFileOptions [
	options := aFileOptions
]

{ #category : 'accessing - file' }
GsFileAdaptor >> position [
	^self file position ifNil: [ self error: 'position error' ]
]

{ #category : 'accessing - file' }
GsFileAdaptor >> position: aPosition [
	(self file position: aPosition) ifNil: [ self error: 'position error' ]
]

{ #category : 'accessing - file' }
GsFileAdaptor >> readInto: readBuffer startingAt: startIndex count: count [
	^(self file read: count into: readBuffer) ifNil: [FileReadError signal: 'File Read Error']
]

{ #category : 'accessing - file' }
GsFileAdaptor >> sync [
	"The handle is always nil. This is here for Pharo compatability"

	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing

	self file sync



	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
	self error: 'sync not yet implmented'

	"fsync() failing cannot be ignored"
]

{ #category : 'accessing - file' }
GsFileAdaptor >> truncate: anInteger [
	self file position: anInteger
]

{ #category : 'accessing - file' }
GsFileAdaptor >> writeFrom: aByteArray startingAt: anIndex count: count [
	^(self nextPutAll: aByteArray) ifNil: [
		(FileWriteError fileName: self fullName) signal: ('File ' , self file name , ' write failed')]
]
