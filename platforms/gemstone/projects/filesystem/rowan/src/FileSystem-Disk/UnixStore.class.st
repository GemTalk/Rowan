"
I'm a specific store for Unix file systems
"
Class {
	#name : 'UnixStore',
	#superclass : 'DiskStore',
	#category : 'FileSystem-Disk-Store'
}

{ #category : 'file constants' }
UnixStore class >> createNewRule [
	"The value used to create a new file"
	^1
]

{ #category : 'public' }
UnixStore class >> delimiter [
	^ $/
]

{ #category : 'file constants' }
UnixStore class >> fileNoShareRule [
	"The file can be shared by others while it is open (ODENYRDWR)"
	^0
]

{ #category : 'file constants' }
UnixStore class >> fileShareReadRule [
	"The file can be read by others while it is open (ODENYWR)"
	^1
]

{ #category : 'file constants' }
UnixStore class >> fileShareWriteRule [
	"The file can be written to others while it is open (ODENYRD)"
	^2
]

{ #category : 'public' }
UnixStore class >> isCaseSensitive [

	^ true
]

{ #category : 'public' }
UnixStore class >> maxFileNameLength [

	^ 255
]

{ #category : 'file constants' }
UnixStore class >> openAlwaysRule [
	"The value used to always open an existing file or create a file if one does not exist"
	^4
]

{ #category : 'file constants' }
UnixStore class >> openExistingRule [
	"The value used to open an existing file"
	^3
]

{ #category : 'public' }
UnixStore class >> platformNames [
	^ {'Linux'}
]

{ #category : 'public' }
UnixStore class >> separator [ 
	^ $:
]

{ #category : 'public' }
UnixStore >> checkName: aFileName fixErrors: fixing [
	"Check if the file name contains any invalid characters"
	| fName |
	fName := super checkName: aFileName fixErrors: fixing.
	
	(fName includes: self delimiter) ifFalse:
		[^fName].
	
	fixing ifFalse: [self error:'Invalid file name'].
	
	^ fName copyReplaceAll: self delimiter asString with: '#'
]
