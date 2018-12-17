Extension { #name : 'GsFile' }

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> truncateTo: anInteger [

	self setPositionTo: anInteger
	
	"work needs to be done here to call out to the bash shell script 'truncate -s anInteger' "

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
GsFile class >> rename: oldFileFullName to: newFileFullName [
	^(self renameFileOnServer: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]

]
