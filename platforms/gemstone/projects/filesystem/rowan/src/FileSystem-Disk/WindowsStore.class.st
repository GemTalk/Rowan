"
I'm a specific store for Windows file systems
"
Class {
	#name : 'WindowsStore',
	#superclass : 'DiskStore',
	#instVars : [
		'disk'
	],
	#classVars : [
		'Disks'
	],
	#category : 'FileSystem-Disk-Store'
}

{ #category : 'file constants' }
WindowsStore class >> createNewRule [
	"The value used to create a new file"
	^1
]

{ #category : 'accessing' }
WindowsStore class >> delimiter [
	^ $\
]

{ #category : 'file constants' }
WindowsStore class >> fileNoShareRule [
	"The file can be shared by others while it is open (ODENYRDWR)"
	^0
]

{ #category : 'file constants' }
WindowsStore class >> fileShareReadRule [
	"The file can be read by others while it is open (ODENYWR)"
	^1
]

{ #category : 'file constants' }
WindowsStore class >> fileShareWriteRule [
	"The file can be written to others while it is open (ODENYRD)"
	^2
]

{ #category : 'file constants' }
WindowsStore class >> genericReadRule [
	"Returns platform specific read value"
	^2147483648
]

{ #category : 'file constants' }
WindowsStore class >> genericReadWriteRule [
	"Returns platform specific read/write value"
	^self genericRead bitOr: self genericWriteRule
]

{ #category : 'file constants' }
WindowsStore class >> genericWriteRule [
	"Returns platform specific write value"
	^1073741824
]

{ #category : 'file constants' }
WindowsStore class >> invalidHandleValue [
	"The value is returned if a file handle was not created"
	^4294967295
]

{ #category : 'accessing' }
WindowsStore class >> isCaseSensitive [
	^ false
]

{ #category : 'accessing' }
WindowsStore class >> maxFileNameLength [
	self flag: 'TODO: more tests needed here!'.
	^ 255
]

{ #category : 'file constants' }
WindowsStore class >> openAlwaysRule [
	"The value used to always open an existing file or create a file if one does not exist"
	^4
]

{ #category : 'file constants' }
WindowsStore class >> openExistingRule [
	"The value used to open an existing file"
	^3
]

{ #category : 'accessing' }
WindowsStore class >> platformNames [
	^ {'Win32' . 'Windows NT'}
]

{ #category : 'accessing' }
WindowsStore class >> separator [ 
	^ $\
]

{ #category : 'public' }
WindowsStore >> checkName: aFileName fixErrors: fixing [
	"Check if the file name contains any invalid characters"
	
	| fName badChars |
	fName := super checkName: aFileName fixErrors: fixing.
	badChars := (#( $: $< $> $| $/ $\ $? $* $"), ((0 to: 31) collect: [:n | n asCharacter])) asSet.
	
	(fName includesAnyOf: badChars)
		ifFalse: [^ fName].
	
	fixing ifFalse: [^self error: 'filename contains invalid characters'].
	
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue: [$#] 
				ifFalse: [char]]
]

{ #category : 'converting' }
WindowsStore >> currentDisk [
	^ disk ifNil: [  disk := FileSystem workingDirectory path segments first ]
]

{ #category : 'converting' }
WindowsStore >> pathFromString: aString [
	"Need to distinguish '' and '/' , so tag latter with invalid character ':'  "
	| normalized pathClass pathElements |
	normalized := aString copy replaceAll: UnixStore delimiter with: self delimiter.
	pathElements := self delimiter split: normalized.
	pathClass := (Path isAbsoluteWindowsPath: normalized)
		ifTrue: [ (normalized = self delimiter asString) ifTrue: [ pathElements := { ':' } ].
			AbsolutePath ]
		ifFalse: [ self stripDrive: pathElements.
			RelativePath ].
	^pathClass withAll: pathElements
]

{ #category : 'converting' }
WindowsStore >> printPath: aPath on: aStream [
	| hasDrive |
	aPath isRoot
		ifTrue: [ ^ self ].	"effectively Windows root is empty string"
	aPath isWorkingDirectory
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath isRelative
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath segments first first = $:
		ifTrue: [ ^ aStream nextPut: self delimiter ].	"as tagged in #pathFromString:  "
	hasDrive := aPath segments first second = $:.
	(hasDrive not )
		ifTrue: [ aStream nextPut: self delimiter ].
	aPath printOn: aStream delimiter: self delimiter.
	(hasDrive and: [ aPath segments size = 1 ])
		ifTrue: [ aStream nextPut: self delimiter ]
]

{ #category : 'converting' }
WindowsStore >> stripDrive: pathElements [
	pathElements ifNotEmpty: [ pathElements at: 1 put: ( ($: split: pathElements first) last)  ]
]
