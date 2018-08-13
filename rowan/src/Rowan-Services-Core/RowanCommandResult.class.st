"
Simple object with the results of the Jadeite command.
"
Class {
	#name : 'RowanCommandResult',
	#superclass : 'Object',
	#instVars : [
		'executionTime'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanCommandResult class >> addResult: aResult [
	aResult command: nil. 
	self results add: aResult

]

{ #category : 'initailize' }
RowanCommandResult class >> cleanupResults [

	"remove the key so users other than SystemUser can
	create a key in their own UserGlobals"

	UserGlobals removeKey: #rowanCommandResults
]

{ #category : 'initailize' }
RowanCommandResult class >> initializeResults [

	UserGlobals at: #rowanCommandResults put: Array new
]

{ #category : 'instance creation' }
RowanCommandResult class >> new [

	| inst |
	inst := super new initialize.
	self addResult: inst.
	^inst

]

{ #category : 'accessing' }
RowanCommandResult class >> results [

	^UserGlobals at: #rowanCommandResults
]

{ #category : 'accessing' }
RowanCommandResult >> command: anObject [

	"results don't have commands"

]

{ #category : 'initialization' }
RowanCommandResult >> initialize [

]

{ #category : 'private' }
RowanCommandResult >> rowanFixMe [

]
