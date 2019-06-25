Class {
	#name : 'RowanDictionaryService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'classes',
		'hierarchyServices'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanDictionaryService >> classHierarchy [
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	"reuse behavior in package service for now" 
	hierarchyServices := (RowanPackageService new classes: classes) classHierarchy: theClasses. 
	(name isKindOf: String) ifFalse:[self halt]. 
	RowanCommandResult addResult: self.
]

{ #category : 'accessing' }
RowanDictionaryService >> name [
	^name
]

{ #category : 'accessing' }
RowanDictionaryService >> name: object [
	name := object
]

{ #category : 'perform' }
RowanDictionaryService >> servicePerform: symbol withArguments: collection [	

	super servicePerform: symbol withArguments: collection.
	self update.
]

{ #category : 'updates' }
RowanDictionaryService >> update [ 
	| dictionary | 
	classes := Array new. 
	dictionary := System myUserProfile symbolList objectNamed: name. 
	dictionary keysAndValuesDo:[:className :theClass |
		theClass isClass ifTrue:[
			| classService | 
			classService :=  RowanClassService new name: className asString. 
			classService versions: theClass classHistory size.
			classService version: (theClass classHistory indexOf: theClass).
			classes add: classService. 
 ]].
(name isKindOf: String) ifFalse:[self halt]. 
	RowanCommandResult addResult: self
]
