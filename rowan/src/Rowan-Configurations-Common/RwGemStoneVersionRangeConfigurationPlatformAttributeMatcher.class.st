Class {
	#name : 'RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher',
	#superclass : 'RwGemStoneVersionConfigurationPlatformAttributeMatcher',
	#instVars : [
		'pattern2'
	],
	#category : 'Rowan-Configurations-Common'
}

{ #category : 'matching' }
RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher >> matchVersion: aGemStoneVersion [

	^ patternMatchBlock cull: pattern cull: aGemStoneVersion cull: pattern2

]

{ #category : 'accessing' }
RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher >> pattern2: anObject [
	pattern2 := anObject

]
