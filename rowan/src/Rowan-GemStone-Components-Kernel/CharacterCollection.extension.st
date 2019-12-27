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
