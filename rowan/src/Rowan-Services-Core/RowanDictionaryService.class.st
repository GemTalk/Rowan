Class {
	#name : 'RowanDictionaryService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'classes',
		'hierarchyServices',
		'globals'
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
	| dictionary sorted | 
	classes := Array new. 
	sorted := SortedCollection sortBlock: [:x :y | x first < y first].  
	dictionary := System myUserProfile symbolList objectNamed: name. 
	dictionary keysAndValuesDo:[:key :value |
		value isClass ifTrue:[
			| classService | 
			classService :=  RowanClassService new name: key asString. 
			classService versions: value classHistory size.
			classService version: (value classHistory indexOf: value).
			classes add: classService. 
		]
		ifFalse:[
			sorted add: (Array 	with: (name, '.', key)
										with: value class name
										with: value asOop
										with: value printString
										)
		]
	].
	globals := sorted asArray. 
	RowanCommandResult addResult: self
]
