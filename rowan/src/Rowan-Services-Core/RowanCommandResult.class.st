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
RowanCommandResult class >> addResult: service [
	service command: nil;
			commandArgs: nil. 
	self updateClientBoundServices: service.
	^service
]

{ #category : 'accessing' }
RowanCommandResult class >> basicAddResult: service [
	self results add: service
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
RowanCommandResult class >> removeResult: aResult [

	self results remove: aResult
]

{ #category : 'accessing' }
RowanCommandResult class >> results [

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanCommandResults ifAbsentPut: [Array new]
]

{ #category : 'private' }
RowanCommandResult class >> updateClientBoundServices: clientBoundService [
	"We're about to add a service to the results collection. 
	That service will be sent to the client. Since services
	are not canonical, we need to do some housekeeping
	to ensure that we don't already have this service 
	somewhere in other client-bound services"
	(self results includes: clientBoundService) ifTrue:[
		self removeResult: clientBoundService].
	self basicAddResult: clientBoundService. 
	self results do:[:service |
		service updateInternalService: clientBoundService.
		clientBoundService updateInternalService: service].
]

{ #category : 'accessing' }
RowanCommandResult >> command: anObject [

	"results don't have commands"

]

{ #category : 'accessing' }
RowanCommandResult >> commandArgs: anObject [

	"results don't have commandArgs"
]

{ #category : 'initialization' }
RowanCommandResult >> initialize [

]

{ #category : 'testing' }
RowanCommandResult >> isMethodService [

	^false
]

{ #category : 'private' }
RowanCommandResult >> rowanFixMe [

]

{ #category : 'accessing' }
RowanCommandResult >> updateInternalService: service [
]
