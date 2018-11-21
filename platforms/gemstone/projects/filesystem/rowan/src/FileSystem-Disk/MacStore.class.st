"
I'm a specific store for OSX file systems
"
Class {
	#name : 'MacStore',
	#superclass : 'UnixStore',
	#category : 'FileSystem-Disk-Store'
}

{ #category : 'current' }
MacStore class >> isActiveClass [
	^ ((System gemVersionAt: 'osName') = 'OSX') and: [ super isActiveClass ]
]

{ #category : 'public' }
MacStore class >> isCaseSensitive [

	^ true
]
