Extension { #name : 'AbstractException' }

{ #category : '*rowan-gemstone-kernel-32x' }
AbstractException >> addText: aString [
  messageText ifNil:[ self asString "build the default messageText" ].
  messageText := (messageText ifNil:[ String new ]), ', ', aString .
]
