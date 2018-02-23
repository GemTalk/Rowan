Class {
	#name : 'GsExploreInteraction',
	#superclass : 'GsInspectInteraction',
	#category : 'GemStone-Interactions'
}

{ #category : 'interacting' }
GsExploreInteraction >> interactWith: anObject [
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithExplore: self
]
