"
I represent an absolute path (a position starting from Path root)
"
Class {
	#name : 'AbsolutePath',
	#superclass : 'Path',
	#category : 'FileSystem-Path'
}

{ #category : 'as yet unclassified' }
AbsolutePath class >> addEmptyElementTo: result [
]

{ #category : 'as yet unclassified' }
AbsolutePath class >> from: aString delimiter: aDelimiterCharater [
	aString = '/'
		ifTrue: [ ^ self root ].
		
	^ super from: aString delimiter: aDelimiterCharater
]

{ #category : 'printing' }
AbsolutePath >> asString [
  "used by topaz stack display"
  | str sz |
  str := '/' copy .
  1 to: (sz := self size) - 1 do:[:j | str addAll: (self at: j) ; add: $/ ].
  str add: (self at: sz ).
  ^ str
]

{ #category : 'testing' }
AbsolutePath >> isAbsolute [
	^ true
]

{ #category : 'testing' }
AbsolutePath >> isRoot [
	^ self size = 0
]

{ #category : 'printing' }
AbsolutePath >> printOn: aStream [
	aStream nextPutAll: 'Path'.
	self isRoot
		ifTrue: [aStream nextPutAll: ' root']
		ifFalse:
			[1 to: self size do:
				[:i |
				aStream 
					nextPutAll: ' / ''';
				 	nextPutAll: (self at: i);
					nextPut: $']]
]

{ #category : 'printing' }
AbsolutePath >> printPathOn: aStream delimiter: aCharacter [
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter

]

{ #category : 'enumerating' }
AbsolutePath >> withParents [
	^ super withParents addFirst: (Path root); yourself
]
