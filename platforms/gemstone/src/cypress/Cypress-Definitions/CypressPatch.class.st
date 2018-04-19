"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPatch',
	#superclass : 'CypressObject',
	#instVars : [
		'operations'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressPatch class >> fromBase: baseSnapshot toTarget: targetSnapshot [
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot

]

{ #category : 'applying' }
CypressPatch >> applyTo: aCypressLoader [
	operations do: [:ea | ea applyTo: aCypressLoader].

]

{ #category : 'initialization' }
CypressPatch >> fromBase: baseSnapshot toTarget: targetSnapshot [
	| base target |	
	operations := OrderedCollection new.
	base := CypressDefinitionIndex definitions: baseSnapshot definitions.
	target := CypressDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions do:
		[:t |
		base
			definitionLike: t
			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]
			ifAbsent: [operations add: (CypressAddition of: t)]].
		
	base definitions do:
		[:b |
		target
			definitionLike: b
			ifPresent: [:t | ]
			ifAbsent: [operations add: (CypressRemoval of: b)]]

]

{ #category : 'accessing' }
CypressPatch >> operations [

	^operations

]
