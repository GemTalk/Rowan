Class {
	#name : 'ClientStore',
	#superclass : 'DiskStore',
	#instVars : [
		'store',
		'defaultWorkingDirectory'
	],
	#classInstVars : [
		'CurrentFS',
		'DefaultWorkingDirectory'
	],
	#category : 'FileSystem-Disk'
}

{ #category : 'current' }
ClientStore class >> currentFileSystem [
	self currentFS ifNil: [self initializeClientFS].
	^self currentFS
]

{ #category : 'accessing' }
ClientStore class >> currentFS [
	^CurrentFS
]

{ #category : 'accessing' }
ClientStore class >> currentFS: aFileSystem [
	CurrentFS := aFileSystem
]

{ #category : 'accessing' }
ClientStore class >> defaultWorkingDirectory [
	^self currentFs store defaultWorkingDirectory
]

{ #category : 'class initialization' }
ClientStore class >> initialize [
]

{ #category : 'class initialization' }
ClientStore class >> initializeClientFS [

	self currentFS: (FileSystem store: ClientStore new)
]

{ #category : 'accessing' }
ClientStore class >> osName [
	^System clientOsName
]

{ #category : 'current' }
ClientStore class >> reset [
	self initializeClientFS
]

{ #category : 'private' }
ClientStore >> basicCreationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"
	| dt |
	dt := DateAndTime posixSeconds:  ((anEntry at: 2) ifNil: [0]) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
]

{ #category : 'private' }
ClientStore >> basicEntriesIn: aPath [
	^self error: 'Not implemented for the client'
]

{ #category : 'public' }
ClientStore >> basicEntryAt: aPath [
	| anEntryType |
	"This is required becuase GemStone's lookup method only works on the server"
	anEntryType := self fileTypeOf: aPath.
	(anEntryType  isNil or: [anEntryType  > 1]) ifTrue: [^#badDirectoryPath].
	^{
		aPath basename.
		nil.
		nil.
		anEntryType = 1.
		nil.
		nil.
		nil }
]

{ #category : 'private' }
ClientStore >> basicIsSymlink: anEntry [
	^(anEntry size >= 7)
		ifTrue: [ (anEntry at: 7) ifNil: [false] ]
		ifFalse: [ false ]
]

{ #category : 'private' }
ClientStore >> basicModificationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"

	| dt |
	dt := DateAndTime posixSeconds:  ((anEntry at: 3) ifNil: [0]) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
]

{ #category : 'accessing' }
ClientStore >> defaultWorkingDirectory [
	^defaultWorkingDirectory
]

{ #category : 'accessing' }
ClientStore >> defaultWorkingDirectory: aPath [
	defaultWorkingDirectory := aPath
]

{ #category : 'accessing' }
ClientStore >> defaultWorkingDirectoryString [
	"Answer the default working directory, which is defined as the directory where the image resides."
	^System clientEnvironmentVariable: self store workingDirectoryEnvironmentVariableName
]

{ #category : 'wip' }
ClientStore >> delimiter [
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ self store delimiter
]

{ #category : 'error handling' }
ClientStore >> errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: anErrorGroup [

	^self store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: anErrorGroup
]

{ #category : 'error handling' }
ClientStore >> errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: fileOptions errorGroup: anErrorGroup [

	^self store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: fileOptions errorGroup: anErrorGroup
]

{ #category : 'error handling' }
ClientStore >> errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference errorGroup: anErrorGroup [

	^anErrorGroup errorClassForDefaultPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference
]

{ #category : 'public' }
ClientStore >> exists: aPath [
	| result |
	result := self fileTypeOf: aPath.
	^result notNil and: [result < 2]
]

{ #category : 'private' }
ClientStore >> fileTypeOf: aPath [
	^FileSystem fileClass _fileKind: (self store stringFromPath: aPath) onClient: true
]

{ #category : 'initialize' }
ClientStore >> initialize [
	super initialize.
	self store: self class activeClass new.
	self defaultWorkingDirectory: (Path from: self defaultWorkingDirectoryString delimiter: self delimiter)
]

{ #category : 'testing' }
ClientStore >> isForeignFileSystem [
	^true
]

{ #category : 'testing' }
ClientStore >> isWindowsStoreClient [
	^self store class name = #WindowsStore
]

{ #category : 'converting' }
ClientStore >> pathFromString: aString [	

	^self store pathFromString: aString
]

{ #category : 'converting' }
ClientStore >> printPath: aPath on: out [
	
	^self store printPath: aPath on: out
]

{ #category : 'accessing' }
ClientStore >> store [
	^store
]

{ #category : 'accessing' }
ClientStore >> store: aDiskStore [
	store := aDiskStore
]
