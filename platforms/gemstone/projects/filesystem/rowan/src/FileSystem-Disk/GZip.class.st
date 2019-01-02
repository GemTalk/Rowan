Class {
	#name : 'GZip',
	#superclass : 'CompressionType',
	#instVars : [
		'compressionLevel'
	],
	#category : 'FileSystem-Disk'
}

{ #category : 'instance creation' }
GZip class >> high [
	^self new 
		compressionLevel: '9'; 
		yourself
]

{ #category : 'instance creation' }
GZip class >> low [
	^self new 
		compressionLevel: '1'; 
		yourself
]

{ #category : 'instance creation' }
GZip class >> none [
	^self new
]

{ #category : 'accessing' }
GZip >> compressionLevel [
	^compressionLevel
]

{ #category : 'accessing' }
GZip >> compressionLevel: anObject [
	compressionLevel := anObject
]

{ #category : 'initialize' }
GZip >> initialize [
	super initialize.
	self compressionLevel: ''
]

{ #category : 'testing' }
GZip >> isGzipped [
	^true
]

{ #category : 'accessing' }
GZip >> printOpenModeStringOn: aStream [

	aStream nextPutAll: self compressionLevel
]
