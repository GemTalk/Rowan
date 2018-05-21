Class {
	#name : 'GsNotifyInteraction',
	#superclass : 'GsConfirmInteraction',
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'accessing' }
GsNotifyInteraction >> defaultActionBlock [
  "notify answers ok by default, while confirm answers fals by default"

  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      Transcript cr; show: self printString.
      self okResponse ] ].
  ^ defaultActionBlock
]
