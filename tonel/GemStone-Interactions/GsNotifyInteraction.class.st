Class {
	#name : 'GsNotifyInteraction',
	#superclass : 'GsConfirmInteraction',
	#category : 'GemStone-Interactions'
}

{ #category : 'accessing' }
GsNotifyInteraction >> defaultActionBlock [
  "notify answers ok by default, while confirm answers fals by default"

  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      (ObjectLogEntry interaction: self printString object: self) addToLog.
      self okResponse ] ].
  ^ defaultActionBlock
]
