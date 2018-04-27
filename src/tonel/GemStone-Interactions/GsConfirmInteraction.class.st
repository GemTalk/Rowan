Class {
	#name : 'GsConfirmInteraction',
	#superclass : 'GsInteraction',
	#instVars : [
		'prompt',
		'confirm',
		'cancel',
		'abort'
	],
	#category : 'GemStone-Interactions'
}

{ #category : 'instance creation' }
GsConfirmInteraction class >> prompt: prompt [
  ^ self prompt: prompt confirm: 'Ok'
]

{ #category : 'instance creation' }
GsConfirmInteraction class >> prompt: prompt confirm: confirm [
  ^ self prompt: prompt confirm: confirm cancel: 'Cancel'
]

{ #category : 'instance creation' }
GsConfirmInteraction class >> prompt: prompt confirm: confirm cancel: cancel [
  ^ self new
    prompt: prompt;
    confirm: confirm;
    cancel: cancel
]

{ #category : 'instance creation' }
GsConfirmInteraction class >> prompt: prompt confirm: confirm cancel: cancel abort: abort [
  "on confirm return true, on cancel return false on abort return nil"

  ^ self new
    prompt: prompt;
    confirm: confirm;
    cancel: cancel;
    abort: abort
]

{ #category : 'accessing' }
GsConfirmInteraction >> abort [
  ^ abort
]

{ #category : 'accessing' }
GsConfirmInteraction >> abort: anObject [
  abort := anObject
]

{ #category : 'confirm' }
GsConfirmInteraction >> abortFor: anInteractionRequest [
  anInteractionRequest response: self abortResponse
]

{ #category : 'confirm' }
GsConfirmInteraction >> abortResponse [
  ^ nil
]

{ #category : 'accessing' }
GsConfirmInteraction >> cancel [
  ^ cancel
]

{ #category : 'accessing' }
GsConfirmInteraction >> cancel: anObject [
	cancel := anObject
]

{ #category : 'confirm' }
GsConfirmInteraction >> cancelFor: anInteractionRequest [
  anInteractionRequest response: self cancelResponse
]

{ #category : 'confirm' }
GsConfirmInteraction >> cancelResponse [
  ^ false
]

{ #category : 'accessing' }
GsConfirmInteraction >> confirm [
	^ confirm
]

{ #category : 'accessing' }
GsConfirmInteraction >> confirm: anObject [
	confirm := anObject
]

{ #category : 'accessing' }
GsConfirmInteraction >> defaultActionBlock [
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
		self halt. 
      Transcript cr; show: self printString.
      self cancelResponse ] ].
  ^ defaultActionBlock

]

{ #category : 'interacting' }
GsConfirmInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithConfirm: self
]

{ #category : 'confirm' }
GsConfirmInteraction >> ok [
  ^ true
]

{ #category : 'confirm' }
GsConfirmInteraction >> okFor: anInteractionRequest [
  anInteractionRequest response: self okResponse
]

{ #category : 'confirm' }
GsConfirmInteraction >> okResponse [
  ^ true
]

{ #category : 'printing' }
GsConfirmInteraction >> printLabel [
  ^ self prompt
]

{ #category : 'accessing' }
GsConfirmInteraction >> prompt [
	^ prompt
]

{ #category : 'accessing' }
GsConfirmInteraction >> prompt: aString [
  prompt := aString copyWrappedTo: 80
]
