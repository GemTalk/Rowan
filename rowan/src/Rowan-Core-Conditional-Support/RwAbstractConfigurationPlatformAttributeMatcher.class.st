Class {
	#name : 'RwAbstractConfigurationPlatformAttributeMatcher',
	#superclass : 'Object',
	#instVars : [
		'pattern',
		'patternMatchBlock'
	],
	#category : 'Rowan-Core-Conditional-Support'
}

{ #category : 'matching' }
RwAbstractConfigurationPlatformAttributeMatcher >> match: anObject [

	self subclassResponsibility: #match:

]

{ #category : 'matching' }
RwAbstractConfigurationPlatformAttributeMatcher >> matchString: aString [

	self subclassResponsibility: #matchString:

]

{ #category : 'matching' }
RwAbstractConfigurationPlatformAttributeMatcher >> matchVersion: anRwGemStoneVersionNumber [

	self subclassResponsibility: #matchVersion:

]

{ #category : 'accessing' }
RwAbstractConfigurationPlatformAttributeMatcher >> pattern: anObject [
	pattern := anObject

]

{ #category : 'accessing' }
RwAbstractConfigurationPlatformAttributeMatcher >> patternMatchBlock: aBlock [
	patternMatchBlock := aBlock

]
