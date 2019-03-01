Extension { #name : 'GsFileAdaptor' }

{ #category : '*FileSystem-Client-Core' }
GsFileAdaptor class >> _fileKind: aPathName onClient: aBoolean [
	^GsFile _fileKind: aPathName onClient: aBoolean
]

{ #category : '*FileSystem-Client-Core' }
GsFileAdaptor class >> createClientDirectory: path [
	^GsFile createClientDirectory: path
]

{ #category : '*FileSystem-Client-Core' }
GsFileAdaptor class >> deleteClientDirectory: path [
	^GsFile _removeDirectory: path onClient: true 
]

{ #category : '*FileSystem-Client-Core' }
GsFileAdaptor class >> deleteClientFile: aPathName [
	^GsFile _removeFile: aPathName onClient: true
]

{ #category : '*FileSystem-Client-Core' }
GsFileAdaptor class >> renameOnClient: oldFileFullName to: newFileFullName [
	^(GsFile renameFile: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]
]
