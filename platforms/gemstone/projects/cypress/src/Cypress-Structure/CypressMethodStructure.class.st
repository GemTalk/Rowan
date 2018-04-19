"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressMethodStructure',
	#superclass : 'CypressStructure',
	#instVars : [
		'source',
		'isMetaclass',
		'classStructure'
	],
	#category : 'Cypress-Structure'
}

{ #category : 'instance creation' }
CypressMethodStructure class >> fromMethodDefinition: methodDefinition [

	^self new
		fromMethodDefinition: methodDefinition;
		yourself

]

{ #category : 'converting' }
CypressMethodStructure >> asCypressMethodDefinition [
	"Try to coerce Unicode source to simple Strings when possible."

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source asString

]

{ #category : 'accessing' }
CypressMethodStructure >> category [

	^self properties at: 'category'

]

{ #category : 'accessing' }
CypressMethodStructure >> category: aString [

	self properties at: 'category' put: aString

]

{ #category : 'accessing' }
CypressMethodStructure >> classStructure [
	^classStructure

]

{ #category : 'accessing' }
CypressMethodStructure >> classStructure: aCypressClassStructure [
	classStructure := aCypressClassStructure

]

{ #category : 'initialization' }
CypressMethodStructure >> fromMethodDefinition: methodDefinition [

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.

]

{ #category : 'accessing' }
CypressMethodStructure >> isMetaclass [

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass

]

{ #category : 'accessing' }
CypressMethodStructure >> isMetaclass: aBoolean [
	isMetaclass := aBoolean

]

{ #category : 'accessing' }
CypressMethodStructure >> selector [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self name
		do: [:chara | stream nextPut: (chara = $. ifTrue: [$:] ifFalse: [chara])].
	^stream contents

]

{ #category : 'accessing' }
CypressMethodStructure >> selector: aString [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	aString
		do: [:chara | stream nextPut: (chara = $: ifTrue: [$.] ifFalse: [chara])].
	name := stream contents

]

{ #category : 'accessing' }
CypressMethodStructure >> source [

	^source

]

{ #category : 'accessing' }
CypressMethodStructure >> source: aString [

	source := aString

]
