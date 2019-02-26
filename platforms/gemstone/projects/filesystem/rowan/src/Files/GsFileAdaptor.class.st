Class {
	#name : 'GsFileAdaptor',
	#superclass : 'AbstractFileAdaptor',
	#category : 'Files-Core'
}

{ #category : 'accessing' }
GsFileAdaptor class >> _entriesIn: pathString [
	| result |
	result := GsFile _contentsOfServerDirectory: pathString expandPath: true.
	^result _isArray ifTrue: [result] ifFalse:  [nil]
]

{ #category : 'client' }
GsFileAdaptor class >> _fileKind: aPathName onClient: aBoolean [
	^GsFile _fileKind: aPathName onClient: aBoolean
]

{ #category : 'accessing' }
GsFileAdaptor class >> _fullPath: path filename: basename [
	"If this is a directory, then the path needs to have a slash on the end"
	| fullPath |
	fullPath := path , '/' , basename.
	(self isDirectory: fullPath) == true ifTrue: [^fullPath , '/'].
	^fullPath
]

{ #category : 'client' }
GsFileAdaptor class >> createClientDirectory: path [
	^GsFile createClientDirectory: path
]

{ #category : 'accessing' }
GsFileAdaptor class >> createDirectory: aPathName [
	^GsFile createServerDirectory: aPathName
]

{ #category : 'accessing' }
GsFileAdaptor class >> currentWorkingDirectoryPath [

	^GsFile _expandEnvVariable: 'PWD' isClient:false
]

{ #category : 'client' }
GsFileAdaptor class >> deleteClientDirectory: path [
	^GsFile _removeDirectory: path onClient: true 
]

{ #category : 'accessing' }
GsFileAdaptor class >> deleteClientFile: aPathName [
	^GsFile _removeFile: aPathName onClient: true
]

{ #category : 'accessing' }
GsFileAdaptor class >> deleteDirectory: aPathName [
	^GsFile removeServerDirectory: aPathName
]

{ #category : 'accessing' }
GsFileAdaptor class >> deleteFile: aPathName [
	^GsFile removeServerFile: aPathName
]

{ #category : 'convenience' }
GsFileAdaptor class >> doesNotUnderstand: aMessage [
	"Redirect message to the resolved version of this GsFile."
	(GsFile respondsTo: aMessage selector)
		ifTrue: [ ^ GsFile perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
]

{ #category : 'accessing' }
GsFileAdaptor class >> entriesIn: pathString [
	| result |
	result := self _entriesIn: pathString.
	^(result ifNil: [#()])
		reject: [:ea | (ea  endsWith: '.') or: [ea endsWith: '..']]
]

{ #category : 'accessing' }
GsFileAdaptor class >> isDirectory: aPathName [
	^GsFile _isDirectory: aPathName onClient: false
]

{ #category : 'accessing' }
GsFileAdaptor class >> lookupDirectory: path filename: basename [

	| fullPath|
	fullPath := self _fullPath: path filename: basename.
	^self lookupDirectoryEntry: fullPath
]

{ #category : 'accessing' }
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
	^aGsFile ifNotNil: [self on: aGsFile]
]

{ #category : 'accessing' }
GsFileAdaptor class >> rename: oldFileFullName to: newFileFullName [
	^(GsFile renameFileOnServer: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]
]

{ #category : 'accessing' }
GsFileAdaptor class >> renameOnClient: oldFileFullName to: newFileFullName [
	^(GsFile renameFile: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]
]

{ #category : 'convenience' }
GsFileAdaptor class >> serverErrorString [
	^GsFile serverErrorString
]

{ #category : 'convenience' }
GsFileAdaptor >> at: anInteger [
	self file at: anInteger
]

{ #category : 'accessing' }
GsFileAdaptor >> atEnd: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^self file atEnd
]

{ #category : 'convenience' }
GsFileAdaptor >> close [
	self file close
]

{ #category : 'accessing' }
GsFileAdaptor >> close: handle [
	"The handle is always nil. This is here for Pharo compatability"

	self file close
]

{ #category : 'convenience' }
GsFileAdaptor >> doesNotUnderstand: aMessage [
	"Redirect message to the resolved version of this GsFile."
	(self file respondsTo: aMessage selector)
		ifTrue: [ ^ self file perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
]

{ #category : 'convenience' }
GsFileAdaptor >> fileSize [
	^self file fileSize
]

{ #category : 'accessing' }
GsFileAdaptor >> flush: handle [
	"The handle is always nil. This is here for Pharo compatability"

	self file flush
]

{ #category : 'accessing' }
GsFileAdaptor >> fullName [
	^self file pathName
]

{ #category : 'accessing' }
GsFileAdaptor >> getPosition: handle [
	"The handle is always nil. This is here for Pharo compatability"
		^self position ifNil: [ self error: 'position error' ]
]

{ #category : 'accessing' }
GsFileAdaptor >> isClosed: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^self file isOpen not
]

{ #category : 'accessing' }
GsFileAdaptor >> isOpen [
	"The handle is always nil. This is here for Pharo compatability"

	^self file isOpen
]

{ #category : 'convenience' }
GsFileAdaptor >> next: count into: buf [
	^self file next: count into: buf
]

{ #category : 'convenience' }
GsFileAdaptor >> nextPutAll: stringOrByteArray [
	^self file nextPutAll: stringOrByteArray
]

{ #category : 'convenience' }
GsFileAdaptor >> position: anInteger [
	^self file position: anInteger
]

{ #category : 'accessing' }
GsFileAdaptor >> read: handle into: readBuffer startingAt: startIndex count: count [
	"The handle is always nil. This is here for Pharo compatability"

	^(self file read: count into: readBuffer) ifNil: [FileReadError signal: 'File Read Error']
]

{ #category : 'accessing' }
GsFileAdaptor >> setPosition: handle to: aPosition [
	"The handle is always nil. This is here for Pharo compatability"
		(self file position: aPosition) ifNil: [ self error: 'position error' ]
]

{ #category : 'accessing' }
GsFileAdaptor >> sizeOf: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^(self fileSize) ifNil: [ self error: 'file size error' ]
]

{ #category : 'accessing' }
GsFileAdaptor >> sizeOrNil: id [

	^self fileSize
]

{ #category : 'accessing' }
GsFileAdaptor >> sync: handle [
	"The handle is always nil. This is here for Pharo compatability"

	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing

	self file sync



	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
	self error: 'sync not yet implmented'

	"fsync() failing cannot be ignored"
]

{ #category : 'accessing' }
GsFileAdaptor >> truncate: handle to: anInteger [

	self setPosition: handle to: anInteger
	
	"work needs to be done here to call out to the bash shell script 'truncate -s anInteger' "
]

{ #category : 'accessing' }
GsFileAdaptor >> write: handle from: aByteArray startingAt: anIndex count: amount [
	"The handle is always nil. This is here for Pharo compatability"

	^(self nextPutAll: aByteArray) ifNil: [
		(FileWriteError fileName: self fullName) signal: ('File ' , self file name , ' write failed')]
]
