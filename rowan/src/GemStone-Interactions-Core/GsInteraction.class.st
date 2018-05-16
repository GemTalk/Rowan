Class {
	#name : 'GsInteraction',
	#superclass : 'Object',
	#instVars : [
		'defaultActionBlock'
	],
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'accessing' }
GsInteraction >> defaultActionBlock [
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
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
