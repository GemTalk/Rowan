Class {
	#name : 'FileNotAvailablePosition',
	#superclass : 'FileStartingPosition',
	#category : 'FileSystem-Disk'
}

{ #category : 'testing' }
FileNotAvailablePosition >> isPositionable [
	^false
]
