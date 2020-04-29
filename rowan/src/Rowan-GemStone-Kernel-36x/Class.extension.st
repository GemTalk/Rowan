Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-36x' }
Class >> _rwOptionsArray [
  ^ self _optionsArrayForDefinition
]

{ #category : '*rowan-gemstone-kernel-36x' }
Class >> _rwReservedOop [
 "returns nil or the SmallInteger specifying a reserved oop"
  ^ self asOopNumber <= System _lastReservedOopNumber 
    ifTrue:[ self asOop ] 
    ifFalse:[ nil ].
]
