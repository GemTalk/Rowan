Class {
	#name : 'RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher',
	#superclass : 'RwGemStoneVersionConfigurationPlatformAttributeMatcher',
	#instVars : [
		'pattern2'
	],
	#category : 'Rowan-Components-Common'
}

{ #category : 'matching' }
RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher >> matchVersion: aGemStoneVersion [

	^ patternMatchBlock cull: pattern cull: aGemStoneVersion cull: pattern2

]

{ #category : 'accessing' }
RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher >> pattern2: anObject [
	pattern2 := anObject

]
