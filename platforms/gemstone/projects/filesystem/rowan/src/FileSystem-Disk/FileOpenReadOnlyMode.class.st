"
Open the file for reading only
"
Class {
	#name : 'FileOpenReadOnlyMode',
	#superclass : 'FileOpenMode',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenReadOnlyMode class >> posixName [
	^'O_RDONLY'
]

{ #category : 'accessing' }
FileOpenReadOnlyMode class >> posixValue [
	^16r00000
]

{ #category : 'testing' }
FileOpenReadOnlyMode >> isReadable [
	^true
]

{ #category : 'accessing' }
FileOpenReadOnlyMode >> mode [
	^'r'
]
