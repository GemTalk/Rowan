Class {
	#name : 'RwStringConfigurationPlatformAttributeMatcher',
	#superclass : 'RwAbstractConfigurationPlatformAttributeMatcher',
	#category : 'Rowan-Configurations-Common'
}

{ #category : 'matching' }
RwStringConfigurationPlatformAttributeMatcher >> match: anObject [

	^ anObject rwPlatformAttributeMatchForString: self

]

{ #category : 'matching' }
RwStringConfigurationPlatformAttributeMatcher >> matchString: aString [

	^ patternMatchBlock cull: pattern cull: aString

]

{ #category : 'matching' }
RwStringConfigurationPlatformAttributeMatcher >> matchVersion: aGemStoneVersion [

	^ false

]
