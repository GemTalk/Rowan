"
I'm a specific store for OSX file systems
"
Class {
	#name : 'MacStore',
	#superclass : 'UnixStore',
	#category : 'FileSystem-Disk'
}

{ #category : 'public' }
MacStore class >> isCaseSensitive [

	^ true
]

{ #category : 'public' }
MacStore class >> platformNames [
	^ {'Darwin'}
]
