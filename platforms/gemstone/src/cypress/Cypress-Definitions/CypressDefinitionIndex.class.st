"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressDefinitionIndex',
	#superclass : 'CypressObject',
	#instVars : [
		'definitionMap'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressDefinitionIndex class >> definitions: aCollection [
	^ self new addAll: aCollection

]

{ #category : 'adding' }
CypressDefinitionIndex >> add: aDefinition [
	^ self definitionMap at: aDefinition description put: aDefinition

]

{ #category : 'adding' }
CypressDefinitionIndex >> addAll: aCollection [
	aCollection do: [:ea | self add: ea]

]

{ #category : 'querying' }
CypressDefinitionIndex >> definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock [
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]

]

{ #category : 'accessing' }
CypressDefinitionIndex >> definitionMap [
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap

]

{ #category : 'accessing' }
CypressDefinitionIndex >> definitions [
	^self definitionMap values

]

{ #category : 'removing' }
CypressDefinitionIndex >> remove: aDefinition [
	self definitionMap removeKey: aDefinition description ifAbsent: []

]
