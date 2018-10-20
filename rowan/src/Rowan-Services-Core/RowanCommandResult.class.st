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
RowanCommandResult class >> initializeResults [

	SessionTemps current at: #rowanCommandResults put: Array new.
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

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanCommandResults ifAbsentPut: [Array new]
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
