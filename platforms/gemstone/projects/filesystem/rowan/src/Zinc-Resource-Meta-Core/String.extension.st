Extension { #name : 'String' }

{ #category : '*Zinc-Resource-Meta-Core' }
String >> addedToZnUrl: url [
	| segments |
	segments := self findTokens: '/'.
	^ url withPathSegments: segments

]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> asUrl [
	^ self asZnUrl

]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> asZnMimeType [
	^ ZnMimeType fromString: self

]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> asZnUrl [
	^ ZnUrl fromString: self

]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> findDelimiters: delimiters startingAt: start [ 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | 
			delim = (self at: i) 
				ifTrue: [^ i]]].
	^ self size + 1

]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> findTokens: delimiters [
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens := OrderedCollection new.
	separators := delimiters isCharacter 
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: separators startingAt: keyStop.
		keyStop := self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens

]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> indexOfSubCollection: sub [ 
	""

	^ self
		indexOfSubCollection: sub
		startingAt: 1
		ifAbsent: [0]
]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock [
	| index |
	index := self findString: sub startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index
]

{ #category : '*Zinc-Resource-Meta-Core' }
String >> skipDelimiters: delimiters startingAt: start [ 
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1

]
