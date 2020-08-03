Extension { #name : 'CharacterCollection' }

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> asRwGemStoneVersionNumber [

	^ RwGemStoneVersionNumber fromString: self

]

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> asRwSemanticVersionNumber [

	^ RwSemanticVersionNumber fromString: self

]

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher [

	^ anRwGemStoneVersionConfigurationPlatformAttributeMatcher matchString: self

]

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher [

	^ anRwStringConfigurationPlatformAttributeMatcher matchString: self

]

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> rwSemanticIntegerLessThanSelf: anInteger [

	"integers have greater precedence than strings"
	"anInteger < aString-> true"

  ^ true

]

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> rwSemanticStringLessThanSelf: aString [

	^ aString < self

]

{ #category : '*rowan-gemstone-components-kernel' }
CharacterCollection >> rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent [

	^ aRwSemanticVersonComponent rwSemanticStringLessThanSelf: self

]

{ #category : '*rowan-gemstone-components-kernel' }
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
