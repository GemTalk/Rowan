Class {
	#name : 'GsInteractionRequest',
	#superclass : 'Notification',
	#instVars : [
		'interaction'
	],
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'interacting' }
GsInteractionRequest class >> signal: aGsInteraction [
  ^ (self new interaction: aGsInteraction) signal
]

{ #category : 'signaling' }
GsInteractionRequest >> defaultAction [
	^ self interaction defaultActionFor: self
]

{ #category : 'Compatibility' }
GsInteractionRequest >> gsArguments [
  "This method included here for G/S 2.x only ... not needed nor used in 3.x"

  ^ {(self interaction)}
]

{ #category : 'Instance initialization' }
GsInteractionRequest >> initialize [
  super initialize.
  gsNumber := 121001
]

{ #category : 'accessing' }
GsInteractionRequest >> interaction [

   "Return the value of the instance variable 'interaction'."
   ^interaction
]

{ #category : 'accessing' }
GsInteractionRequest >> interaction: anObject [

   "Modify the value of the instance variable 'interaction'."
   interaction := anObject
]

{ #category : 'signaling' }
GsInteractionRequest >> response: anObject [
  self resume: anObject
]
