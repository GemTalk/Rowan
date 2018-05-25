"
Simple object with the results of the Jadeite command.
"
Class {
	#name : 'RowanCommandResult',
	#superclass : 'Object',
	#instVars : [
		'executionTime'
	],
	#classVars : [
		'Results'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanCommandResult class >> addResult: aResult [

	self results add: aResult

]

{ #category : 'initailize' }
RowanCommandResult class >> initializeResults [

	Results := Array new

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

	^Results ifNil:[Results := Array new]

]

{ #category : 'initialization' }
RowanCommandResult >> initialize [

]
