"
GsInteraction enableInteractions.
GsInteraction disableInteractions.
GsInteraction interactionsEnabled.

(GsTextInteraction prompt: 'prompt') signal.
(GsTextInteraction prompt: 'prompt with template'  template: 'template') signal.
(GsTextInteraction requestPassword: 'password') signal.
	
No handler available in JadeiteForPharo
"
Class {
	#name : 'GsTextInteraction',
	#superclass : 'GsInteraction',
	#instVars : [
		'prompt',
		'template',
		'requestPassword'
	],
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'instance creation' }
GsTextInteraction class >> prompt: aString [
  ^ self prompt: aString template: ''
]

{ #category : 'instance creation' }
GsTextInteraction class >> prompt: promptString template: templateString [
  ^ self new
    prompt: promptString;
    template: templateString;
    yourself
]

{ #category : 'instance creation' }
GsTextInteraction class >> requestPassword: aString [
  ^ self new
    requestPassword: aString;
    yourself
]

{ #category : 'accessing' }
GsTextInteraction >> defaultActionBlock [
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      '' ] ].
  ^ defaultActionBlock
]

{ #category : 'interacting' }
GsTextInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithText: self
]

{ #category : 'printing' }
GsTextInteraction >> printLabel [
  ^ self prompt
]

{ #category : 'accessing' }
GsTextInteraction >> prompt [

   "Return the value of the instance variable 'prompt'."
   ^prompt
]

{ #category : 'accessing' }
GsTextInteraction >> prompt: aString [
  prompt := aString copyWrappedTo: 80
]

{ #category : 'accessing' }
GsTextInteraction >> requestPassword [

   requestPassword ifNil: [ requestPassword := false ].
   ^requestPassword
]

{ #category : 'accessing' }
GsTextInteraction >> requestPassword: aString [

   prompt := aString.
   requestPassword := true
]

{ #category : 'accessing' }
GsTextInteraction >> template [
  template ifNil: [ template := '' ].
  ^ template
]

{ #category : 'accessing' }
GsTextInteraction >> template: anObject [

   "Modify the value of the instance variable 'template'."
   template := anObject
]
