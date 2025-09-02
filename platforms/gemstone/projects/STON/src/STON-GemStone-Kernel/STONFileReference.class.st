"
Part of STON (not FileSystem)

===

This utility class is used to support serializing FileReference instances for disk-based file systems and in-memory file systems. STON expects a single serialization/deserialization format per class. To match Pharo's behavior, we need to serialize disk-based file systems specially. This class handles this special treatment.

Do not create instances of this class.
"
Class {
	#name : 'STONFileReference',
	#superclass : 'FileReference',
	#category : 'STON-GemStone-Kernel'
}

{ #category : 'ston' }
STONFileReference class >> fromSton: stonReader [

	^stonReader parseListSingleton asFileReference
]

{ #category : 'ston' }
STONFileReference class >> stonName [

	^#FILE
]
