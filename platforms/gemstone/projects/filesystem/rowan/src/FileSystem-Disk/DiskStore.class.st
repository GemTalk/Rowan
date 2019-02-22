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
		'CurrentFS',
		'DefaultWorkingDirectory'
	],
	#classInstVars : [
		'PosixValues',
		'PosixErrorNumbers'
	],
	#category : 'FileSystem-Disk-Store'
}

{ #category : 'current' }
DiskStore class >> activeClass [
	^DiskStore activeClassFor: self osName
]

{ #category : 'current' }
DiskStore class >> activeClassFor: anOsName [
	self allSubclasses do: [:ea | 
		(ea platformNames includes: anOsName) ifTrue: [^ ea] ].
	^ self
]

{ #category : 'class initialization' }
DiskStore class >> allConcreteFilePosixErrorClasses [
	^FilePosixError allConcreteFilePosixErrorClasses
]

{ #category : 'class initialization' }
DiskStore class >> allConcretePosixFlagClasses [
	^PosixFlag allConcretePosixFlagClasses
]

{ #category : 'current' }
DiskStore class >> createDefault [
	^ self new
]

{ #category : 'create rules' }
DiskStore class >> createNewRule [
	FileOptionFeatureNotSupported signal: 'createNewRule'
]

{ #category : 'current' }
DiskStore class >> current [
	^ self currentFileSystem store
]

{ #category : 'current' }
DiskStore class >> currentFileSystem [
	^ CurrentFS ifNil: [
		CurrentFS := FileSystem store: self activeClass createDefault]
]

{ #category : 'accessing' }
DiskStore class >> defaultWorkingDirectory [
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."

	DefaultWorkingDirectory ifNil: [DefaultWorkingDirectory := FileSystem fileClass currentWorkingDirectoryPath].
	^(Path from: DefaultWorkingDirectory delimiter: self delimiter).
]

{ #category : 'public' }
DiskStore class >> delimiter [
	^ self current delimiter
]

{ #category : 'share rules' }
DiskStore class >> denyNoneShareRule [
	FileOptionFeatureNotSupported signal: 'denyNoneShareRule'
]

{ #category : 'share rules' }
DiskStore class >> denyReadShareRule [
	FileOptionFeatureNotSupported signal: 'denyReadShareRule'
]

{ #category : 'share rules' }
DiskStore class >> denyReadWriteShareRule [
	FileOptionFeatureNotSupported signal: 'denyReadWriteShareRule'
]

{ #category : 'share rules' }
DiskStore class >> denyWriteShareRule [
	FileOptionFeatureNotSupported signal: 'denyWriteShareRule'
]

{ #category : 'class initialization' }
DiskStore class >> initialize [
	self initializePosixFlagValues.
	self initializePosixErrorValues
]

{ #category : 'class initialization' }
DiskStore class >> initializeDefaultWorkingDirectory [
	^DefaultWorkingDirectory := self defaultWorkingDirectory
]

{ #category : 'class initialization' }
DiskStore class >> initializePosixErrorValues [
	" DiskStore initializePosixErrorValues"
	| errorNumberSelector |
	errorNumberSelector := ((self name copyFrom: 1 to: self name size - 5) , 'ErrorNumber') asValidSelector.
	self posixErrorNumbers: Dictionary new. 
	self allConcreteFilePosixErrorClasses do: [:eaClass |  
			self posixErrorNumbers at: eaClass posixName put: (eaClass errorNumberForSelector: errorNumberSelector)]
]

{ #category : 'class initialization' }
DiskStore class >> initializePosixFlagValues [
	" DiskStore initializePosixFlagValues "
	| valueSelector |
	valueSelector := ((self name copyFrom: 1 to: self name size - 5) , 'Value') asValidSelector.
	self posixValues: Dictionary new. 
	self allConcretePosixFlagClasses do: [:eaClass |  
			self posixValues at: eaClass posixName put: (eaClass valueForSelector: valueSelector)]
]

{ #category : 'public' }
DiskStore class >> maxFileNameLength [
	self subclassResponsibility 
]

{ #category : 'create rules' }
DiskStore class >> noTruncateRule [
	FileOptionFeatureNotSupported signal: 'noTruncateRule'
]

{ #category : 'create rules' }
DiskStore class >> openAlwaysRule [
	FileOptionFeatureNotSupported signal: 'openAlwaysRule'
]

{ #category : 'create rules' }
DiskStore class >> openExistingRule [
	FileOptionFeatureNotSupported signal: 'openExistingRule'
]

{ #category : 'posix' }
DiskStore class >> optionPlatformValueFor: aPosixName [

	^self posixValues at: aPosixName ifAbsent: []
]

{ #category : 'current' }
DiskStore class >> platformNames [
	^{}
]

{ #category : 'accessing' }
DiskStore class >> posixErrorNumbers [
	^PosixErrorNumbers
]

{ #category : 'accessing' }
DiskStore class >> posixErrorNumbers: aCollection [
	PosixErrorNumbers := aCollection
]

{ #category : 'accessing' }
DiskStore class >> posixValues [
	^PosixValues
]

{ #category : 'accessing' }
DiskStore class >> posixValues: aCollection [
	PosixValues := aCollection
]

{ #category : 'access rules' }
DiskStore class >> readOnlyAccessRule [
	FileOptionFeatureNotSupported signal: 'readOnlyAccessRule'
]

{ #category : 'access rules' }
DiskStore class >> readWriteAccessRule [
	FileOptionFeatureNotSupported signal: 'readWriteAccessRule'
]

{ #category : 'current' }
DiskStore class >> reset [
	self posixValues: nil.
	CurrentFS := nil.
	DefaultWorkingDirectory := nil
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

{ #category : 'create rules' }
DiskStore class >> truncateExistingRule [
	FileOptionFeatureNotSupported signal: 'truncateExistingRule'
]

{ #category : 'access rules' }
DiskStore class >> writeOnlyAccessRule [
	FileOptionFeatureNotSupported signal: 'readWriteAccessRule'
]

{ #category : 'public' }
DiskStore >> _createDirectory: path [
	^FileSystem fileClass createDirectory: path.
]

{ #category : 'public' }
DiskStore >> _deleteDirectory: pathString [
	^FileSystem fileClass deleteDirectory: pathString
]

{ #category : 'public' }
DiskStore >> _deleteFile: pathString [
	^FileSystem fileClass deleteFile: pathString
]

{ #category : 'public' }
DiskStore >> _rename: sourcePathString to: targetPathString [
	^FileSystem fileClass rename: sourcePathString to: targetPathString
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
DiskStore >> basicEntriesIn: aPath [
	^FileSystem fileClass entriesIn: aPath
]

{ #category : 'private' }
DiskStore >> basicEntry: ignored path: aPath nodesDo: aBlock [

	| pathString |
		
	pathString := self stringFromPath: aPath.
	(self basicEntriesIn: pathString) do: [:eaEntry |
		aBlock value: (FileSystem fileClass lookupDirectoryEntry: eaEntry)]
]

{ #category : 'public' }
DiskStore >> basicEntryAt: aPath [
	| path basename |
	
	path := self stringFromPath: aPath parent.
	basename := aPath basename.
	
	^ (FileSystem fileClass lookupDirectory: path filename: basename)
		ifNil: [ #badDirectoryPath ].
]

{ #category : 'private' }
DiskStore >> basicIsDirectory: anEntry [

	^anEntry at: 4
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
DiskStore >> basicOpen: aPath options: aFileOptions [

	| string |
	string := self stringFromPath: aPath.
	^FileSystem fileClass openPathName: string withOptions: aFileOptions
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
	result := self _createDirectory: pathString.
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
DiskStore >> defaultWorkingDirectory [
	"Answer the default working directory, which is defined as the directory where the image resides."
 
	DefaultWorkingDirectory  ifNil: [self class initializeDefaultWorkingDirectory].
	^DefaultWorkingDirectory
]

{ #category : 'public' }
DiskStore >> delete: path [
	| pathString |
	
	(self exists: path) 
		ifFalse: [ ^ FileDoesNotExistException signalWith: path ].
		
	pathString := self stringFromPath: path.
	
	(self isDirectory: path)
		ifTrue: [ self deleteDirectory: pathString ]
		ifFalse: [ self deleteFile: pathString ]
]

{ #category : 'public' }
DiskStore >> deleteDirectory: pathString [
	(self _deleteDirectory: pathString)
		ifNil: [ self error: 'Error deleting directory ', pathString printString, ' :: ', FileSystem fileClass serverErrorString ]
]

{ #category : 'public' }
DiskStore >> deleteFile: pathString [
	(self _deleteFile: pathString)
		ifNil: [ FileDeletionError 
					signalWith: pathString  
					text: 'Could not delete file ' , pathString , '. Check the file is not open.' ]
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
	maxFileNameLength := 255
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
	(self basicOpen: aPath options: self class currentFileSystem readFileOptions) 
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
	(self basicOpen: aPath options: self class currentFileSystem writeFileOptions) 
		ifNotNil: [ :id|
			id close.
			^ true].
	^ false
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

{ #category : 'posix' }
DiskStore >> optionPlatformValueFor: aPosixName [

	^self class optionPlatformValueFor: aPosixName
]

{ #category : 'public' }
DiskStore >> rename: sourcePath to: destinationPath [ 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	| sourcePathString targetPathString |
	sourcePathString := self stringFromPath: sourcePath.
	targetPathString := self stringFromPath: destinationPath.
	^self _rename: sourcePathString to: targetPathString
]

{ #category : 'private' }
DiskStore >> rootNode [
	^ #('' 0 0 true 0 8r555)
]

{ #category : 'accessing' }
DiskStore >> workingDirectoryEnvironmentVariableName [
	^'PWD'
]
