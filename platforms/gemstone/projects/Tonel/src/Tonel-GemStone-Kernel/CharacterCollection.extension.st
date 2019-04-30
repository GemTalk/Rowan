Extension { #name : 'CharacterCollection' }

{ #category : '*tonel-gemstone-kernel' }
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

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> findString: subString startingAt: startIndex caseSensitive: aBoolean [

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not

]

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> putOn: aStream [

	^ aStream nextPutAll: self

]

{ #category : '*tonel-gemstone-kernel' }
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

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> trimBoth [

	"Trim separators from both sides of the receiving string."

	^ self trimBoth: [ :char | char isSeparator ]

]

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> trimBoth: aBlock [

	"Trim characters satisfying the condition given in aBlock from both sides of the receiving string."

	^ self trimLeft: aBlock right: aBlock

]

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> trimLeft [

	"Trim separators from the left side of the receiving string."

	^ self trimLeft: [ :char | char isSeparator ]

]

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> trimLeft: aBlock [

	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."

	^ self trimLeft: aBlock right: [ :char | false ]

]

{ #category : '*tonel-gemstone-kernel' }
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

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> trimRight [

	"Trim separators from the right side of the receiving string."

	^ self trimRight: [ :char | char isSeparator ]

]

{ #category : '*tonel-gemstone-kernel' }
CharacterCollection >> trimRight: aBlock [

	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock

]
