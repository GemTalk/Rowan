Extension { #name : 'Object' }

{ #category : '*rowan-gemstone-components-kernel' }
Object >> rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher [
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'

]

{ #category : '*rowan-gemstone-components-kernel' }
Object >> rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher [
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'

]

{ #category : '*rowan-gemstone-components-kernel' }
Object >> rwSemanticIntegerLessThanSelf: anInteger [
  ^ self
    error: 'Invalid semantic verson component - should be an Integer.'

]

{ #category : '*rowan-gemstone-components-kernel' }
Object >> rwSemanticStringLessThanSelf: aString [
  ^ self
    error: 'Invalid semantic verson component - should be String.'

]

{ #category : '*rowan-gemstone-components-kernel' }
Object >> rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent [
  ^ self
    error: 'Invalid semantic verson component - should be String or Integer.'

]
