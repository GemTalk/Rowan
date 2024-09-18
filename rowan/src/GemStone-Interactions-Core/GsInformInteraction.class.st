"
Returns true when 'OK'  button pressed.

GsInteraction enableInteractions.
GsInteraction disableInteractions.
GsInteraction interactionsEnabled.
self inform: 'message'.

Handler available in JadeiteForPharo
"
Class {
	#name : 'GsInformInteraction',
	#superclass : 'GsInteraction',
	#instVars : [
		'message'
	],
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'instance creation' }
GsInformInteraction class >> message: aString [
  ^ self new
    message: aString;
    yourself
]

{ #category : 'interacting' }
GsInformInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithInform: self
]

{ #category : 'accessing' }
GsInformInteraction >> message [

   "Return the value of the instance variable 'message'."
   ^message
]

{ #category : 'accessing' }
GsInformInteraction >> message: aString [
  message := aString copyWrappedTo: 80
]

{ #category : 'printing' }
GsInformInteraction >> printLabel [
  ^ self message
]
