Class {
	#name : 'RowanDictionaryService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'classes'
	],
	#category : 'Rowan-Services-Core'
}

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
			classes add: (RowanClassService minimalForClassNamed: className asString) ]].
	RowanCommandResult addResult: self
]
