Extension { #name : 'CharacterCollection' }

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> endsWith: suffix [

	"Answer whether the tail end of the receiver is the same as suffix.
	 The comparison is case-sensitive."

	| ofs |
	suffix size == 0
		ifTrue: [ ^ false ].
	(ofs := self size - suffix size) < 0
		ifTrue: [ ^ false ].
	^ self at: ofs + 1 equals: suffix	"
  'Elvis' endsWith: 'vis'
  'Elvis' endsWith: ''
"

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> findString: subString startingAt: startIndex caseSensitive: aBoolean [

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> join: aCollection [ 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
	^ self class new: (aCollection size * self size) streamContents: [:stream | 
			aCollection
				do: [:each | stream nextPutAll: each asString] 
				separatedBy: [stream nextPutAll: self]]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> lineIndicesDo: aBlock [
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| cr lf start sz nextLF nextCR |
	start := 1.
	sz := self size.
	cr := Character cr.
	nextCR := self indexOf: cr startingAt: 1.
	lf := Character lf.
	nextLF := self indexOf: lf startingAt: 1.
	[ start <= sz ] whileTrue: [
		(nextLF = 0 and: [ nextCR = 0 ])
			ifTrue: [ "No more CR, nor LF, the string is over"
					aBlock value: start value: sz value: sz.
					^self ].
		(nextCR = 0 or: [ 0 < nextLF and: [ nextLF < nextCR ] ])
			ifTrue: [ "Found a LF"
					aBlock value: start value: nextLF - 1 value: nextLF.
					start := 1 + nextLF.
					nextLF := self indexOf: lf startingAt: start ]
			ifFalse: [ 1 + nextCR = nextLF
				ifTrue: [ "Found a CR-LF pair"
					aBlock value: start value: nextCR - 1 value: nextLF.
					start := 1 + nextLF.
					nextCR := self indexOf: cr startingAt: start.
					nextLF := self indexOf: lf startingAt: start ]
				ifFalse: [ "Found a CR"
					aBlock value: start value: nextCR - 1 value: nextCR.
					start := 1 + nextCR.
					nextCR := self indexOf: cr startingAt: start ]]]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> putOn: aStream [

	^ aStream nextPutAll: self

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> substrings: separators [ 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream subStringStream |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	subStringStream := String new writeStreamPortable.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				subStringStream isEmpty ifFalse: [
					result add: subStringStream contents.
					subStringStream := String new writeStreamPortable ] ]
			ifFalse: [
				subStringStream nextPut: char ] ].
	subStringStream isEmpty ifFalse: [
		result add: subStringStream contents ].
	^ result asArray

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimBoth [

	"Trim separators from both sides of the receiving string."

	^ self trimBoth: [ :char | char isSeparator ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimBoth: aBlock [

	"Trim characters satisfying the condition given in aBlock from both sides of the receiving string."

	^ self trimLeft: aBlock right: aBlock

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimLeft [

	"Trim separators from the left side of the receiving string."

	^ self trimLeft: [ :char | char isSeparator ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimLeft: aBlock [

	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."

	^ self trimLeft: aBlock right: [ :char | false ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimLeft: aLeftBlock right: aRightBlock [

	"Trim characters satisfying the condition given in aLeftBlock from the left side and aRightBlock from the right sides of the receiving string."

	| left right |
	left := 1.
	right := self size.
	[ left <= right and: [ aLeftBlock value: (self at: left) ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ aRightBlock value: (self at: right) ] ]
		whileTrue: [ right := right - 1 ].
	^ self copyFrom: left to: right

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimRight [

	"Trim separators from the right side of the receiving string."

	^ self trimRight: [ :char | char isSeparator ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> trimRight: aBlock [

	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock

]

{ #category : '*rowan-tonel-gemstone-kernel' }
CharacterCollection >> withLineEndings: lineEndingString [
	| stream |
	stream := nil.
	self
		lineIndicesDo: [ :start :endWithoutDelimiters :end | 
			(stream isNil and: [ endWithoutDelimiters ~= end ])
				ifTrue: [ 
					((self copyFrom: endWithoutDelimiters + 1 to: end)
						_unicodeEqual: lineEndingString)
						ifFalse: [ 
							stream := WriteStreamPortable with: self copy.
							stream position: start - 1 ] ].
			stream
				ifNotNil: [ 
					stream next: endWithoutDelimiters - start + 1 putAll: self startingAt: start.
					endWithoutDelimiters = end
						ifFalse: [ stream nextPutAll: lineEndingString ] ] ].
	^ stream
		ifNil: [ self ]
		ifNotNil: [ 
			stream position = self size
				ifTrue: [ stream originalContents ]
				ifFalse: [ stream contents ] ]
]
