"
I represent a sequential binary File. I provide the minimum operations to:

- move the cursor fo the file
- reading
- writing

I am also the entry point of the FilePlugin primitives.

!Examples of usage

""Creating a file""
file := File named: 'asd.txt' asFileReference fullName.

""Opening / closing it""
file open.
file openForAppend.
file close.

""Accessing the file properties""
file size.
file position.
file position: 0.
file seekAbsolute: 10.
file seekRelative: 10.
file atEnd.

""Writing""
file nextPutAll: 'sdd'.

""Reading""
file next: 2.

""Buffered write""
file next: 2 putAll: 'abc' startingAt: 2.

""Buffered read""
buffer := ByteArray new: 5.
file readInto: buffer startingAt: 1 count: 5.
buffer asString.
"
Class {
	#name : 'File',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'Files-Core'
}

{ #category : 'primitives-file' }
File class >> atEnd: aGsFile [
	"Answer true if the file position is at the end of the file."

	^ aGsFile atEnd
]

{ #category : 'primitives-file' }
File class >> close: aGsFile [
	"Close this file."

	aGsFile ifNil: [ ^ self ].
	aGsFile close
]

{ #category : 'primitives-file' }
File class >> closed: aGsFile [
	^ aGsFile isNil or: [ (self sizeOrNil: aGsFile) isNil ]
]

{ #category : 'primitives-file' }
File class >> connectToFile: filePointer writable: writableFlag [
	"Open the file with the supplied FILE* pointer, and return the file ID obtained.
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened.
	It is the responsibility of the caller to coordinate closing the file."

"
	<primitive: 'primitiveConnectToFile' module: 'FilePlugin' error: error>
	error = #'bad argument' ifTrue: [ 
		(filePointer isKindOf: ByteArray) ifFalse: 
			[ ^self error: 'filePointer must be a ByteArray' ].
		(writableFlag isKindOf: Boolean) ifFalse: 
			[ ^self error: 'writableFlag must be a boolean' ] ].
	^ self primitiveFailed 
"
self error: 'not yet implmented'

]

{ #category : 'primitives-file' }
File class >> connectToFileDescriptor: fileDescriptor writable: writableFlag [
	"Connect to the file with fileDescriptor number, and return the file ID obtained.
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened.
	It is the responsibility of the caller to coordinate closing the file."

"
	<primitive: 'primitiveConnectToFileDescriptor' module: 'FilePlugin' error: error>
	error = #'bad argument' ifTrue: [ 
		fileDescriptor isInteger ifFalse: 
			[ ^self error: 'fileDescriptor must be an integer' ].
		(writableFlag isKindOf: Boolean) ifFalse: 
			[ ^self error: 'writableFlag must be a boolean' ] ].
	^ self primitiveFailed 
"
self error: 'not yet implmented'


]

{ #category : 'primitives-path' }
File class >> createDirectory: fullPath [
	"Create a directory named by the given path. 
	Fail if the path is bad or if a file or directory by that name already exists."

	^ GsFile createServerDirectory: fullPath
]

{ #category : 'primitives-path' }
File class >> deleteDirectory: fullPath [
	"Delete the directory named by the given path. 
	Fail if the path is bad or if a directory by that name does not exist."

	GsFile removeServerDirectory: fullPath
]

{ #category : 'primitives-path' }
File class >> deleteFile: aFileName [
	"Delete the file of the given name. 
	Return self if the primitive succeeds, nil otherwise."

	^ GsFile _removeFile: aFileName onClient: false
]

{ #category : 'primitives-path' }
File class >> delimiter [
	"Return the path delimiter for the underlying platform's file system."

	"Was a primitive ... only needed to resolve whether or not we're on Windows"

	^ $/
]

{ #category : 'primitives-file' }
File class >> fileDescriptorIsAvailable: anInteger [
	"Answer a boolean indicating whether the supplied file descriptor (anInteger) is available.
	A file descriptor is considered available if it is connected to a console / terminal, pipe or a file.  cygwin terminals are currently not supported (the pipe is created, but nothing appears in the terminal)"

	^ (self fileDescriptorType: anInteger) between: 1 and: 3
]

{ #category : 'primitives-file' }
File class >> fileDescriptorType: fdNum [
	"Allow to test if the standard input/output files are from a console or not
	Return values:
	* -1 - Error
	* 0 - no console (windows only)
	* 1 - normal terminal (unix terminal / windows console)
	* 2 - pipe
	* 3 - file
	* 4 - cygwin terminal (windows only)"

"
	<primitive: 'primitiveFileDescriptorType' module: 'FilePlugin' error: error>
	error = #'bad argument'
		ifTrue: [ fdNum isInteger
				ifFalse: [ ^ self error: 'File Descriptor must be an integer!' ] ].
	^ self primitiveFailed
"
self error: 'not yet implmented'

]

{ #category : 'primitives-file' }
File class >> flush: aGsFile [
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their
	own internal buffer to minimize write() syscalls. This flushes that buffer.
	On Windows this and primSync: do the same thing."

	aGsFile flush
]

{ #category : 'primitives-path' }
File class >> getMacFile: fileName type: typeString creator: creatorString [
	"Get the Macintosh file type and creator info for the file with the given name. Fails if the file does not exist or if the type and creator type arguments are not strings of length 4. This primitive is Mac specific; it is a noop on other platforms."

"
 	<primitive: 'primitiveDirectoryGetMacTypeAndCreator' module: 'FilePlugin'>
"
self error: 'not yet implmented'


]

{ #category : 'primitives-file' }
File class >> getPosition: aGsFile [
	"Get this files current position."

	^ aGsFile position
]

{ #category : 'primitives-path' }
File class >> lookupDirectory: fullPath filename: fileName [

	"Look up <fileName> (a simple file name) in the directory identified by <fullPath> and return entry array"

	^ self lookupPath: fullPath, '/', fileName
]

{ #category : 'primitives-path' }
File class >> lookupPath: fullPath [

	"Look up the file or directory identified by <fullPath> and return an array containing:

	<fileName> 			
	<creationTime> 			(in seconds since the start of the Linux time epoch)
	<modificationTime> 	(in seconds since the start of the Linux time epoch)
	<dirFlag> 				DirFlag is true if the entry is a directory
	<fileSize> 				FileSize the file size in bytes or zero for directories
	<posixPermissions> 	Numeric Notation 
	<symLinkFlag>			seemingly, symLinkFlag is true if the entry is a symLink

	On Unix, the empty path denotes '/'. 
   On Macs and PCs, it is the container of the system volumes."

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

{ #category : 'file creation' }
File class >> named: fileName [
	"Open a file with the given name for reading and writing. If the name has no directory part, then the file will be created in the default directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new named: fileName
]

{ #category : 'primitives-file' }
File class >> open: fileName writable: writeMode [
	"Open a file of the given name, and return the GsFile obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	"writeMode - #read, #append, #write"

	| mode |
	mode := writeMode == #write 
		ifTrue: [ 'w+' ] 
		ifFalse: [ 
			writeMode = #read
				ifTrue: [ 'r+' ]
				ifFalse: [ 'a+' ] ].
	^ GsFile
		open: fileName 
		mode: mode 
		onClient: false
]

{ #category : 'file creation' }
File class >> openAndTruncateForWriteFileNamed: aName [

	^ (self named: aName)
		delete;
		openForWrite
]

{ #category : 'file creation' }
File class >> openForReadFileNamed: aName [

	^ (self named: aName)
		openForRead
]

{ #category : 'file creation' }
File class >> openForWriteFileNamed: aName [

	^ (self named: aName) openForWrite
]

{ #category : 'primitives-file' }
File class >> read: aGsFile into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. 
		Answer the number of bytes actually read."

	| buf cnt |
	buf := byteArray species new: count.
	cnt := aGsFile next: count into: buf.
	cnt ifNil: [ 
		(self closed: aGsFile)
			ifTrue: [ ^ self error: 'File is closed' ].
		self error: 'File read failed: ' , (GsFile serverErrorString ifNil: [ '']) ].
	byteArray replaceFrom: startIndex to: startIndex + cnt - 1 with: buf.
	^cnt
]

{ #category : 'primitives-path' }
File class >> rename: oldFileFullName to: newFileFullName [ 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	(GsFile renameFileOnServer: oldFileFullName to: newFileFullName) = 0
		ifFalse: [ self error: 'Rename file failed'].
]

{ #category : 'primitives-file' }
File class >> setPosition: aGsFile to: anInteger [
	"Set this file to the given position."

	| pos |
	pos := aGsFile position: anInteger.
	pos ifNil: [ self error: 'position error' ].
]

{ #category : 'primitives-file' }
File class >> sizeOf: aGsFile [
	"Answer the size of this file."

	^ aGsFile fileSize
]

{ #category : 'primitives-file' }
File class >> sizeOrNil: aGsFile [
	"Answer the size of this file."

	aGsFile ifNil: [ ^ nil ].
	^ aGsFile fileSize
]

{ #category : 'primitives-file' }
File class >> stdioHandles [
"
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed
"
self error: 'not yet implmented'

]

{ #category : 'primitives-file' }
File class >> stdioIsAvailable [
	"Answer a boolean indicating whether stdio is available on the current platform.
	stdio is considered available if any one of the three files (stdin, stdout, stderr) is available."

	^ (0 to: 2)
		anySatisfy: [ :fdNum | self fileDescriptorIsAvailable: fdNum ]
]

{ #category : 'primitives-file' }
File class >> sync: id [
	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing"

"
	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
self error: 'not yet implmented'

	"fsync() failing cannot be ignored"
	self primitiveFailed
]

{ #category : 'primitives-file' }
File class >> truncate: id to: anInteger [
	"Truncate this file to the given position."
"
	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed
"
self error: 'not yet implmented'

]

{ #category : 'primitives-file' }
File class >> write: aGsFile from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. 	Answer the number of bytes written."

	| written |
	written := aGsFile nextPutAll: (stringOrByteArray copyFrom: startIndex to: startIndex + count - 1).
	written ifNil: [ self error: 'failed write' ].
	^ written
]

{ #category : 'accessing' }
File >> basename [
	^self name
]

{ #category : 'open/close' }
File >> basicOpenForWrite: writeMode [ 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."

	"writeMode - #read, #append, #write"

	^ self class open: name writable: writeMode
]

{ #category : 'testing' }
File >> checkDoesNotExist [
	"This method implements a backwards compatible logic to #newFileNamed:
	
	If the file does not exist, this method has no effect, and returns self.
	
	If the file exists, it will throw a FileAlreadyExistsException.
	If unhandled, this will open a pop-up asking the user to enter a new name or to override the existing file.
	In this case, this method returns a new file with the options selected by the user.
	"
	self exists ifTrue: [
		^ FileAlreadyExistsException signalOnFile: self ]
]

{ #category : 'open/close' }
File >> delete [
	"We retries with GC because in some platforms open files cannot be open"
	self class deleteFile: name.
	self exists
		ifTrue: [ (CannotDeleteFileException new messageText: 'Could not delete file ' , name,'. Check the file is not open.') signal ].
]

{ #category : 'testing' }
File >> exists [

	| handle |
	"We open it for read. If the returned handle is nil, the file could not be opened"
	handle := self basicOpenForWrite: #read.
	handle ifNil: [^ false].
	self class close: handle.
	^ true
]

{ #category : 'instance creation' }
File >> name [

	^ name
]

{ #category : 'instance creation' }
File >> named: fileName [

	name := fileName
]

{ #category : 'open/close' }
File >> openForAppend [

	| stream |
	stream := self openForWrite: #append.
	^ stream setToEnd; yourself
]

{ #category : 'open/close' }
File >> openForRead [

	^ self openForWrite: #read
]

{ #category : 'open/close' }
File >> openForWrite [

	^ self openForWrite: #write
]

{ #category : 'open/close' }
File >> openForWrite: writeMode [
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."

	"writeMode - #read, #append, #write"

	| fileHandle |
	fileHandle := self basicOpenForWrite: writeMode.
	fileHandle ifNil: [
		"Opening the file failed.
		If the file does not exist, we throw an explicit FileDoesNotExistException.
		Otherwise, we throw a generic FileException."
		self exists
			ifFalse: [ ^ FileDoesNotExistException signalWithFile: self writeMode: writeMode == #write ].
		CannotDeleteFileException signal: name
	].

	^ BinaryFileStream handle: fileHandle file: self forWrite: writeMode ~~ #read
]

{ #category : 'streaming' }
File >> readStream [
	
	^ self openForRead
]

{ #category : 'streaming' }
File >> readStreamDo: aBlock [
	| stream |
	stream := self readStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]

{ #category : 'accessing' }
File >> size [

	^ self readStreamDo: [ :stream | stream size ]
]

{ #category : 'streaming' }
File >> writeStream [

	^ self openForWrite
]

{ #category : 'streaming' }
File >> writeStreamDo: aBlock [
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]
