"
GsInteraction enableInteractions.
GsInteraction disableInteractions.
GsInteraction interactionsEnabled.

(GsChoiceInteraction 
	prompt: 'prompt' 
	labels: {'label'} 
	values: {'value'} 
	lines: {'line'}) signal.
	
No handler available in JadeiteForPharo
"
Class {
	#name : 'GsChoiceInteraction',
	#superclass : 'GsInteraction',
	#instVars : [
		'prompt',
		'labels',
		'values',
		'lines'
	],
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'instance creation' }
GsChoiceInteraction class >> labels: anArray [
	^ self
		prompt: nil
		labels: anArray
		values: anArray
		lines: #()
]

{ #category : 'instance creation' }
GsChoiceInteraction class >> labels: labelArray lines: lineArray [
	^ self
		prompt: nil
		labels: labelArray
		values: labelArray
		lines: lineArray
]

{ #category : 'instance creation' }
GsChoiceInteraction class >> prompt: aString labels: labelArray values: valueArray [
	^ self
		prompt: aString
		labels: labelArray
		 values: valueArray
		lines: #()
]

{ #category : 'instance creation' }
GsChoiceInteraction class >> prompt: aString labels: labelArray values: valueArray lines: lineArray [
  ^ self new
    prompt: aString;
    labels: labelArray;
    values: valueArray;
    lines: lineArray;
    yourself
]

{ #category : 'interacting' }
GsChoiceInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithChoice: self
]

{ #category : 'accessing' }
GsChoiceInteraction >> labels [

   "Return the value of the instance variable 'labels'."
   ^labels
]

{ #category : 'accessing' }
GsChoiceInteraction >> labels: anObject [

   "Modify the value of the instance variable 'labels'."
   labels := anObject
]

{ #category : 'accessing' }
GsChoiceInteraction >> lines [

   "Return the value of the instance variable 'lines'."
   ^lines
]

{ #category : 'accessing' }
GsChoiceInteraction >> lines: anObject [

   "Modify the value of the instance variable 'lines'."
   lines := anObject
]

{ #category : 'printing' }
GsChoiceInteraction >> printLabel [
  ^ self prompt
]

{ #category : 'accessing' }
GsChoiceInteraction >> prompt [

   "Return the value of the instance variable 'prompt'."
   ^prompt
]

{ #category : 'accessing' }
GsChoiceInteraction >> prompt: aString [
  prompt := aString copyWrappedTo: 80
]

{ #category : 'choice' }
GsChoiceInteraction >> select: anIndex [
  ^ self values at: anIndex
]

{ #category : 'choice' }
GsChoiceInteraction >> select: anIndex for: anInteractionRequest [
  anInteractionRequest response: (self values at: anIndex)
]

{ #category : 'accessing' }
GsChoiceInteraction >> values [

   "Return the value of the instance variable 'values'."
   ^values
]

{ #category : 'accessing' }
GsChoiceInteraction >> values: anObject [

   "Modify the value of the instance variable 'values'."
   values := anObject
]
