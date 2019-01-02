Extension { #name : 'GsFile' }

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> atEnd: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^self atEnd
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> close: handle [
	"The handle is always nil. This is here for Pharo compatability"

	self close

]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> flush: handle [
	"The handle is always nil. This is here for Pharo compatability"

	self flush

]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> fullName [
	^self pathName

]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> getPosition: handle [
	"The handle is always nil. This is here for Pharo compatability"
		^self position ifNil: [ self error: 'position error' ]
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> isClosed: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^self isOpen not

]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> read: handle into: readBuffer startingAt: startIndex count: count [
	"The handle is always nil. This is here for Pharo compatability"

	^(self read: count into: readBuffer) ifNil: [FileReadError signal: 'File Read Error']
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> setPosition: handle to: aPosition [
	"The handle is always nil. This is here for Pharo compatability"
		(self position: aPosition) ifNil: [ self error: 'position error' ]
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> sizeOf: handle [
	"The handle is always nil. This is here for Pharo compatability"

	^(self fileSize) ifNil: [ self error: 'file size error' ]
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> sizeOrNil: id [

	^self fileSize
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> sync: handle [
	"The handle is always nil. This is here for Pharo compatability"

	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing

	self file sync



	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
	self error: 'sync not yet implmented'

	"fsync() failing cannot be ignored"
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> truncate: handle to: anInteger [

	self setPosition: handle to: anInteger
	
	"work needs to be done here to call out to the bash shell script 'truncate -s anInteger' "
]

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> write: handle from: aByteArray startingAt: anIndex count: amount [
	"The handle is always nil. This is here for Pharo compatability"

	^(self nextPutAll: aByteArray) ifNil: [
		(FileWriteError fileName: self fullName) signal: ('File ' , self name , ' write failed')]

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> _entriesIn: pathString [
	| result |
	result := self _contentsOfServerDirectory: pathString expandPath: true.
	^result _isArray ifTrue: [result] ifFalse:  [nil]

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> _fullPath: path filename: basename [
	"If this is a directory, then the path needs to have a slash on the end"
	| fullPath |
	fullPath := path , '/' , basename.
	(self isDirectory: fullPath) == true ifTrue: [^fullPath , '/'].
	^fullPath

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> createDirectory: aPathName [
	^self createServerDirectory: aPathName
]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> currentWorkingDirectoryPath [

	^self _expandEnvVariable: 'PWD' isClient:false

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> deleteDirectory: aPathName [
	^self removeServerDirectory: aPathName

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> deleteFile: aPathName [
	^self removeServerFile: aPathName

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> entriesIn: pathString [
	| result |
	result := self _entriesIn: pathString.
	^(result ifNil: [#()])
		reject: [:ea | (ea  endsWith: '.') or: [ea endsWith: '..']]

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> isDirectory: aPathName [
	^self _isDirectory: aPathName onClient: false

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> lookupDirectory: path filename: basename [

	| fullPath|
	fullPath := self _fullPath: path filename: basename.
	^self lookupDirectoryEntry: fullPath
]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> lookupDirectoryEntry: fullPath [
	| gsFileStat |
	gsFileStat := self stat: fullPath isLstat: true.
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

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> open: aPathName withOptions: aFileOptions [
	"This is the current way of openning a file"
	^self open: aPathName mode: aFileOptions modeString onClient: aFileOptions isClientFileSystem withCompression: aFileOptions isGzipped

	"This section sketcher out how to open the file using POSIX flags"


	""
]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> openReference: aFileReference withOptions: aFileOptions [

	^(self open: aFileReference fullName withOptions: aFileOptions) 
		ifNil: [FileDoesNotExistException signalWith: aFileReference fullName]
]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> rename: oldFileFullName to: newFileFullName [
	^(self renameFileOnServer: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]

]
