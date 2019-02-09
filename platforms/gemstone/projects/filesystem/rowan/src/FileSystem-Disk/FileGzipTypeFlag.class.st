"
GemStone has a gzipped compression option which this class supports.

Gemstone supports levels of: none, 1 and 9.

The main prupose of this class is to ensure the file opening mode string is properly created.
"
Class {
	#name : 'FileGzipTypeFlag',
	#superclass : 'FileTypeFlag',
	#instVars : [
		'compressionLevel'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileGzipTypeFlag class >> defaultPlatformValue [
	^0
]

{ #category : 'instance creation' }
FileGzipTypeFlag class >> high [
	^self new 
		compressionLevel: '9'; 
		yourself
]

{ #category : 'instance creation' }
FileGzipTypeFlag class >> low [
	^self new 
		compressionLevel: '1'; 
		yourself
]

{ #category : 'instance creation' }
FileGzipTypeFlag class >> none [
	^self new
]

{ #category : 'accessing' }
FileGzipTypeFlag class >> posixName [
	^'O_GZIP'
]

{ #category : 'accessing' }
FileGzipTypeFlag >> compressionLevel [
	^compressionLevel ifNil: ['']
]

{ #category : 'accessing' }
FileGzipTypeFlag >> compressionLevel: aString [
	compressionLevel := aString
]

{ #category : 'testing' }
FileGzipTypeFlag >> isGzipped [
	^self compressionLevel = '1' or: [self compressionLevel = '9' ]
]

{ #category : 'printing' }
FileGzipTypeFlag >> printOpenModeStringOn: aStream [
	aStream nextPutAll: self compressionLevel
]
