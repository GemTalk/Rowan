"
A framework for managing multi-client interactions like inspect, confirm:, inform:, and interactive Warning dialog (see the Object and Warning extensions).
The framework has default actions defined for running doing reasonable things when run in a client-less environment like topaz.
Clients can register client forwarder handlers for each of the interactions, so that the client can arrange to have the appropriate dialog come up for a given interaction. See the TDTopezServer extension method for an example of client registrations.
"

Class {
	#name : 'GsInteraction',
	#superclass : 'Object',
	#instVars : [
		'defaultActionBlock'
	],
	#category : 'GemStone-Interactions'
}

{ #category : 'accessing' }
GsInteraction >> defaultActionBlock [
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      (ObjectLogEntry interaction: self printString object: self) addToLog.
      nil ] ].
  ^ defaultActionBlock
]

{ #category : 'accessing' }
GsInteraction >> defaultActionBlock: anObject [

   "Modify the value of the instance variable 'defaultActionBlock'."
   defaultActionBlock := anObject
]

{ #category : 'interacting' }
GsInteraction >> defaultActionFor: anInteractionRequest [
  ^ self defaultActionBlock value: anInteractionRequest
]

{ #category : 'interacting' }
GsInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  self subclassResponsibility
]

{ #category : 'printing' }
GsInteraction >> printLabel [
  ^ ''
]

{ #category : 'printing' }
GsInteraction >> printOn: aStream [
  aStream nextPutAll: self class name asString , '(' , self printLabel , ')'
]

{ #category : 'interacting' }
GsInteraction >> signal [
  ^ GsInteractionRequest signal: self
]
