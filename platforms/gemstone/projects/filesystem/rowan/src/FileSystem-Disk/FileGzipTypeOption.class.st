Class {
	#name : 'FileGzipTypeOption',
	#superclass : 'FileTypeOption',
	#instVars : [
		'compressionLevel'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileGzipTypeOption class >> defaultPlatformValue [
	^0
]

{ #category : 'instance creation' }
FileGzipTypeOption class >> high [
	^self new 
		compressionLevel: '9'; 
		yourself
]

{ #category : 'instance creation' }
FileGzipTypeOption class >> low [
	^self new 
		compressionLevel: '1'; 
		yourself
]

{ #category : 'instance creation' }
FileGzipTypeOption class >> none [
	^self new
]

{ #category : 'accessing' }
FileGzipTypeOption class >> posixName [
	^'O_GZIP'
]

{ #category : 'accessing' }
FileGzipTypeOption >> compressionLevel [
	^compressionLevel ifNil: ['']
]

{ #category : 'accessing' }
FileGzipTypeOption >> compressionLevel: aString [
	compressionLevel := aString
]

{ #category : 'other' }
FileGzipTypeOption >> high [
	^self new 
		compressionLevel: '9'; 
		yourself
]

{ #category : 'testing' }
FileGzipTypeOption >> isGzipped [
	^self compressionLevel = '1' or: [self compressionLevel = '9' ]
]

{ #category : 'printing' }
FileGzipTypeOption >> printOpenModeStringOn: aStream [
	aStream nextPutAll: self compressionLevel
]
