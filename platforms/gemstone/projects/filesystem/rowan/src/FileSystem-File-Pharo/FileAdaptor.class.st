Class {
	#name : 'FileAdaptor',
	#superclass : 'AbstractFileAdaptor',
	#instVars : [
		'handle',
		'options'
	],
	#category : 'FileSystem-File-Pharo'
}

{ #category : 'accessing - file' }
FileAdaptor class >> createDirectory: aPathName [

	^File createDirectory: aPathName
]

{ #category : 'accessing - file' }
FileAdaptor class >> deleteDirectory: aPathName [

	^File deleteDirectory: aPathName
]

{ #category : 'accessing - file' }
FileAdaptor class >> rename: oldFileFullName to: newFileFullName [ 

	File rename: oldFileFullName to: newFileFullName
]

{ #category : 'accessing - file' }
FileAdaptor >> atEnd [
	^ File atEnd: self handle
]

{ #category : 'accessing - file' }
FileAdaptor >> close [
	File close: self handle
]

{ #category : 'accessing - file' }
FileAdaptor >> fileSize [
	^File sizeOf: self handle
]

{ #category : 'accessing - file' }
FileAdaptor >> flush [
	"When writing, this flushes the write buffer the stream uses to reduce
	the number of write() system calls it makes. This should generally be
	used before #sync, but on Windows they do the same thing."
	
	File flush: self handle
]

{ #category : 'accessing' }
FileAdaptor >> handle [
	^handle
]

{ #category : 'accessing' }
FileAdaptor >> handle: aByteArray [
	handle := aByteArray
]

{ #category : 'accessing - file' }
FileAdaptor >> isClosed [
	^(File sizeOrNil: handle) isNil
]

{ #category : 'testing' }
FileAdaptor >> isWritable [
	^self options isWritable
]

{ #category : 'accessing' }
FileAdaptor >> options [
	^options
]

{ #category : 'accessing' }
FileAdaptor >> options: aFileOptions [
	options := aFileOptions
]

{ #category : 'accessing - file' }
FileAdaptor >> position [
	^ File getPosition: handle
]

{ #category : 'accessing - file' }
FileAdaptor >> position: pos [
	"Truncate to this position"

	^ File setPosition: self handle to: pos
]

{ #category : 'accessing - file' }
FileAdaptor >> readInto: readBuffer startingAt: startIndex count: count [
	^File read: self handle into: readBuffer startingAt: startIndex count: count
]

{ #category : 'accessing - file' }
FileAdaptor >> sync [	
	self file sync: self handle
]

{ #category : 'accessing - file' }
FileAdaptor >> truncate: pos [
	^ File truncate: self handle to: pos
]

{ #category : 'accessing - file' }
FileAdaptor >> writeFrom: aByteArray startingAt: anIndex count: count [
	File
		write: self handle
		from: aByteArray
		startingAt: 1
		count: count
]
