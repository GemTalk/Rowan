Class {
	#name : 'RwGemStoneVersionConfigurationPlatformAttributeMatcher',
	#superclass : 'RwAbstractConfigurationPlatformAttributeMatcher',
	#category : 'Rowan-Core-Conditional-Support'
}

{ #category : 'matching' }
RwGemStoneVersionConfigurationPlatformAttributeMatcher >> match: anObject [

	^ anObject rwPlatformAttributeMatchForGemStoneVersion: self

]

{ #category : 'matching' }
RwGemStoneVersionConfigurationPlatformAttributeMatcher >> matchString: aString [

	^ false

]

{ #category : 'matching' }
RwGemStoneVersionConfigurationPlatformAttributeMatcher >> matchVersion: aGemStoneVersion [

	^ patternMatchBlock cull: pattern cull: aGemStoneVersion

]
