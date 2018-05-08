Extension { #name : 'Object' }

{ #category : '*gemstone-interactionskernel' }
Object >> confirm: aString [
  ^ (GsConfirmInteraction prompt: aString) signal
]

{ #category : '*gemstone-interactionskernel' }
Object >> inform: aString [
  "Display aString to the user."

  ^ (GsInformInteraction message: aString) signal
]

{ #category : '*gemstone-interactionskernel' }
Object >> inspect [
  ^ (GsInspectInteraction theObject: self) signal
]
