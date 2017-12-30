Class {
	#name : 'GsInspectInteraction',
	#superclass : 'GsInteraction',
	#instVars : [
		'theObject'
	],
	#category : 'GemStone-Interactions'
}

{ #category : 'instance creation' }
GsInspectInteraction class >> theObject: anObject [
  ^ self new
    theObject: anObject;
    yourself
]

{ #category : 'accessing' }
GsInspectInteraction >> defaultActionBlock [
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      (ObjectLogEntry interaction: self printString object: self) addToLog.
      self theObject ] ].
  ^ defaultActionBlock
]

{ #category : 'interacting' }
GsInspectInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithInspect: self
]

{ #category : 'printing' }
GsInspectInteraction >> printLabel [
  ^ self theObject printString
]

{ #category : 'accessing' }
GsInspectInteraction >> theObject [

   "Return the value of the instance variable 'theObject'."
   ^theObject
]

{ #category : 'accessing' }
GsInspectInteraction >> theObject: anObject [

   "Modify the value of the instance variable 'theObject'."
   theObject := anObject
]
