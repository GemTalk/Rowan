"
GsInteraction enableInteractions.
GsInteraction disableInteractions.
GsInteraction interactionsEnabled.

(GsMultiLineTextInteraction prompt: 'prompt') signal.
(GsMultiLineTextInteraction prompt: 'prompt with template'  template: 'template') signal.
(GsMultiLineTextInteraction requestPassword: 'password') signal.
	
No handler available in JadeiteForPharo
"
Class {
	#name : 'GsMultiLineTextInteraction',
	#superclass : 'GsTextInteraction',
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'interacting' }
GsMultiLineTextInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithMultiLineText: self
]
