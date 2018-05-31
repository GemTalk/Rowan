Class {
	#name : 'RwGemStoneVersionConfigurationPlatformAttributeMatcher',
	#superclass : 'RwAbstractConfigurationPlatformAttributeMatcher',
	#category : 'Rowan-Configurations'
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
