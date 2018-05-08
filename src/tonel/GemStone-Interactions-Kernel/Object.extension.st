Extension { #name : 'Object' }

{ #category : '*gemstone-interactions-kernel' }
Object >> confirm: aString [
  ^ (GsConfirmInteraction prompt: aString) signal
]

{ #category : '*gemstone-interactions-kernel' }
Object >> inform: aString [
  "Display aString to the user."

  ^ (GsInformInteraction message: aString) signal
]

{ #category : '*gemstone-interactions-kernel' }
Object >> inspect [
  ^ (GsInspectInteraction theObject: self) signal
]
