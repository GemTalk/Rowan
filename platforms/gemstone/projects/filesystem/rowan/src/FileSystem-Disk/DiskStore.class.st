"
I am an abstract superclass for disk store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 

"
Class {
	#name : 'DiskStore',
	#superclass : 'FileSystemStore',
	#instVars : [
		'maxFileNameLength'
	],
	#classVars : [
		'CurrentClientFS',
		'CurrentFS',
		'DefaultClientWorkingDirectory',
		'DefaultWorkingDirectory'
	],
	#category : 'FileSystem-Disk-Store'
}

{ #category : 'accessing' }
DiskStore class >> activeClass [
	self allSubclasses do: [:ea | 
		(ea platformNames includes: self osName) ifTrue: [^ ea] ].
	^ self
]

{ #category : 'accessing' }
DiskStore class >> activeClientClass [
	self allSubclasses do: [:ea | 
		(ea platformNames includes: self clientOsName) ifTrue: [^ ea] ].
	^ self
]

{ #category : 'current' }
DiskStore class >> createDefault [
	^ self new
]

{ #category : 'current' }
DiskStore class >> current [
	^ self currentFileSystem store
]

{ #category : 'current' }
DiskStore class >> currentClient [
	^ self currentClientFileSystem store
]

{ #category : 'current' }
DiskStore class >> currentClientFileSystem [
	^ CurrentClientFS ifNil: [
		CurrentClientFS := FileSystem store: self activeClientClass createDefault]
]

{ #category : 'current' }
DiskStore class >> currentFileSystem [
	^ CurrentFS ifNil: [
		CurrentFS := FileSystem store: self activeClass createDefault]
]

{ #category : 'accessing' }
DiskStore class >> defaultClientWorkingDirectory [
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."
	| pathString |
	
	"This is hacked for testing purposes. The retrieval of the client pwd needs to be added to the "
	pathString := (GsFile _expandEnvVariable: 'PWD' isClient:true) ifNil: ['C:\Users\Public\Documents'].
	^(Path from: pathString delimiter: self delimiter).
]

{ #category : 'accessing' }
DiskStore class >> defaultWorkingDirectory [
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."
	| pathString |

	pathString := (GsFile _expandEnvVariable: 'PWD' isClient:false).
	^(Path from: pathString delimiter: self delimiter).
]

{ #category : 'public' }
DiskStore class >> delimiter [
	^ self current delimiter
]

{ #category : 'public' }
DiskStore class >> maxFileNameLength [
	self subclassResponsibility 
]

{ #category : 'current' }
DiskStore class >> platformNames [
	^{}
]

{ #category : 'current' }
DiskStore class >> reset [
	DefaultWorkingDirectory := nil.
	CurrentFS := nil.
	DefaultClientWorkingDirectory := nil.
	CurrentClientFS := nil
]

{ #category : 'system startup' }
DiskStore class >> shutDown: quitting [
	"Force to detect filesystem after image restart"
	self reset
]

{ #category : 'system startup' }
DiskStore class >> startUp: resuming [
	resuming 
		ifTrue: [ self reset ].
	DefaultWorkingDirectory := self defaultWorkingDirectory.
]

{ #category : 'comparing' }
DiskStore >> = other [
	^ self species = other species
]

{ #category : 'private' }
DiskStore >> basenameFromEntry: entry [
	^ entry at: 1
]

{ #category : 'private' }
DiskStore >> basicCreationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"
	| dt |
	dt := DateAndTime posixSeconds:  (anEntry at: 2)  offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
]

{ #category : 'private' }
DiskStore >> basicEntry: ignored path: aPath nodesDo: aBlock [
	| pathString intOrArray |
		
	pathString := self stringFromPath: aPath.
	intOrArray := GsFile _contentsOfServerDirectory: pathString expandPath: true.
	intOrArray _isArray ifFalse: [ ^ self signalDirectoryDoesNotExist: aPath ].
	intOrArray
		do: [:entryPathString |
			((entryPathString endsWith: '.')  or: [ entryPathString endsWith: '..' ])
				ifFalse: [ aBlock value: (self lookupPath: entryPathString) ] ]
]

{ #category : 'public' }
DiskStore >> basicEntryAt: aPath [
	| path basename |
	
	path := self stringFromPath: aPath parent.
	basename := aPath basename.
	
	^ (self lookupDirectory: path filename: basename)
		ifNil: [ #badDirectoryPath ].
]

{ #category : 'private' }
DiskStore >> basicIsDirectory: anEntry [
	^ anEntry at: 4
]

{ #category : 'private' }
DiskStore >> basicIsFile: anEntry [
	^ (anEntry at: 4) not
]

{ #category : 'private' }
DiskStore >> basicIsSymlink: anEntry [
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]
]

{ #category : 'private' }
DiskStore >> basicModificationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"

	| dt |
	dt := DateAndTime posixSeconds:  (anEntry at: 3) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
]

{ #category : 'public' }
DiskStore >> basicOpen: aPath writable: writeMode [

	"writeMode - #read, #append, #write"

	| string |
	string := self stringFromPath: aPath.
	^GsFile  open: string mode: writeMode mode onClient: false
]

{ #category : 'private' }
DiskStore >> basicPosixPermissions: anEntry [
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].
]

{ #category : 'private' }
DiskStore >> basicSizeOf: anEntry [
	^ (anEntry at: 5)
]

{ #category : 'public' }
DiskStore >> checkName: aFileName fixErrors: fixErrors [
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."
	
	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength := self maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixErrors
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].
	^ aFileName
]

{ #category : 'public' }
DiskStore >> createDirectory: path [
	"Create a directory for the argument path. 
	If the path refers to an existing file, raise FileExists.
	If the path refers to an existing directory, raise DirectoryExists.
	If the parent directory of the path does not exist, raise DirectoryDoesNotExist"

	| parent pathString result |
	pathString := self stringFromPath: path.
	result := GsFile createServerDirectory: pathString.
	result
		ifNil: [ 
			parent := path parent.
			(self exists: path)
				ifTrue: [ 
					(self isFile: path)
						ifTrue: [ self signalFileExists: path ]
						ifFalse: [ self signalDirectoryExists: path ] ].
			(self isDirectory: parent)
				ifFalse: [ ^ self signalDirectoryDoesNotExist: parent ].
			self primitiveFailed ].
	^ self
]

{ #category : 'accessing' }
DiskStore >> defaultClientWorkingDirectory [
	"Answer the default working directory, which is defined as the directory where the image resides."

	^ DefaultClientWorkingDirectory
		ifNil: [ self class defaultClientWorkingDirectory ]
]

{ #category : 'accessing' }
DiskStore >> defaultWorkingDirectory [
	"Answer the default working directory, which is defined as the directory where the image resides."

	^ DefaultWorkingDirectory
		ifNil: [ self class defaultWorkingDirectory ]
]

{ #category : 'public' }
DiskStore >> delete: path [
	| pathString |
	
	(self exists: path)
		ifFalse: [ ^ FileDoesNotExist signalWith: path ].
		
	pathString := self stringFromPath: path.
	
	(self isDirectory: path)
		ifTrue: [ self deleteDirectory: pathString ]
		ifFalse: [ self deleteFile: pathString ]
]

{ #category : 'public' }
DiskStore >> deleteDirectory: pathString [
	
	(GsFile removeServerDirectory: pathString)
		ifNil: [ self error: 'Error deleting directory ', pathString printString, ' :: ', GsFile serverErrorString ]
]

{ #category : 'public' }
DiskStore >> deleteFile: pathString [

	(GsFile removeServerFile: pathString)
		ifNil: [ FileDeletionError 
					signalWith: pathString  
					text: 'Could not delete file ' , pathString , '. Check the file is not open.' ].
]

{ #category : 'printing' }
DiskStore >> forReferencePrintOn: aStream [
	aStream nextPutAll: 'File @ '
]

{ #category : 'accessing' }
DiskStore >> handleClass [
	^ FileHandle
]

{ #category : 'comparing' }
DiskStore >> hash [
	^ self species hash
]

{ #category : 'initialization' }
DiskStore >> initialize [
	super initialize.
	maxFileNameLength := 255.
]

{ #category : 'public' }
DiskStore >> isDirectory: aPath [
	| entry |
	
	aPath isRoot ifTrue: [ ^ true ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsDirectory: entry ].
	
]

{ #category : 'testing' }
DiskStore >> isDiskFileSystem [
	^ true
]

{ #category : 'public' }
DiskStore >> isFile: aPath [
	| entry |
	
	aPath isRoot ifTrue: [ ^ false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsFile: entry ].
	
]

{ #category : 'public' }
DiskStore >> isReadable: aPath [
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: self readMode) 
		ifNotNil: [ :id|
			id close.
			^ true].
	^ false
]

{ #category : 'public' }
DiskStore >> isSymlink: aPath [
	| entry |
	
	aPath isRoot ifTrue: [ ^false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsSymlink: entry ].
	
]

{ #category : 'public' }
DiskStore >> isWritable: aPath [
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: self readWriteMode) 
		ifNotNil: [ :id|
			id close.
			^ true].
	^ false
]

{ #category : 'public' }
DiskStore >> lookupDirectory: fullPath filename: fileName [

	"Look up <fileName> (a simple file name) in the directory identified by <fullPath> and return entry array"

	^ self lookupPath: fullPath, '/', fileName
]

{ #category : 'public' }
DiskStore >> lookupPath: fullPath [

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

{ #category : 'public' }
DiskStore >> maxFileNameLength [
	^ maxFileNameLength
]

{ #category : 'private' }
DiskStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	
	| entry |
	
	aPath isRoot ifTrue: [ ^ presentBlock value: self rootNode ].
	
	entry := self basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: absentBlock
		ifFalse: [
			entry at: 1 put: aPath basename.
			presentBlock value: entry ].
]

{ #category : 'public' }
DiskStore >> openFileStream: path writable: writable [
	| fullPath |
	fullPath := self stringFromPath: path.
	"redirect over the default implementation"
	^ writable 
		ifFalse: [ FileStream readOnlyFileNamed: fullPath ]
		ifTrue: [ FileStream fileNamed: fullPath ]
]

{ #category : 'public' }
DiskStore >> rename: sourcePath to: destinationPath [ 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	| sourcePathString targetPathString |
	sourcePathString := self stringFromPath: sourcePath.
	targetPathString := self stringFromPath: destinationPath.
	(GsFile renameFileOnServer: sourcePathString to: targetPathString) = 0
		ifFalse: [ self error: 'Rename file failed']
]

{ #category : 'private' }
DiskStore >> rootNode [
	^ #('' 0 0 true 0 8r555)
]
