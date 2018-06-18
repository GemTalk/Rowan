"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressLoaderMissingClasses',
	#superclass : 'Error',
	#type : 'variable',
	#instVars : [
		'requirementsMap'
	],
	#gs_options : [
		'disallowGciStore'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressLoaderMissingClasses class >> missingRequirementsMap: aDictionary [
	"Answer an instance of the receiver initialized on the specified
	 missing requirements. aDictionary maps prerequisite names to
	 a collection of dependent definitions."

	^self new
		initializeRequirementsMap: aDictionary;
		yourself

]

{ #category : 'initializing - private' }
CypressLoaderMissingClasses >> initialize [

	super initialize.
	gsResumable := true

]

{ #category : 'initializing - private' }
CypressLoaderMissingClasses >> initializeMessageText [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream nextPutAll: 'Missing classes:'.
	self requirementsMap keysAndValuesDo: 
			[:className :definitions |
			stream
				space;
				nextPutAll: className printString , '(' , definitions size printString
							, ')'].
	messageText := stream contents

]

{ #category : 'initializing - private' }
CypressLoaderMissingClasses >> initializeRequirementsMap: aDictionary [

	self
		requirementsMap: aDictionary;
		initializeMessageText.

]

{ #category : 'accessing' }
CypressLoaderMissingClasses >> requirementsMap [
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

   ^requirementsMap

]

{ #category : 'updating' }
CypressLoaderMissingClasses >> requirementsMap: aDictionary [
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

	requirementsMap := aDictionary

]
