Extension { #name : 'Object' }

{ #category : '*gemstone-interactions' }
Object >> confirm: aString [
  ^ (GsConfirmInteraction prompt: aString) signal
]

{ #category : '*gemstone-interactions' }
Object >> inform: aString [
  "Display aString to the user."

  ^ (GsInformInteraction message: aString) signal
]

{ #category : '*gemstone-interactions' }
Object >> inspect [
  ^ (GsInspectInteraction theObject: self) signal
]
