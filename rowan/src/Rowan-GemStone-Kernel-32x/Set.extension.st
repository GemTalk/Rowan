Extension { #name : 'Set' }

{ #category : '*rowan-gemstone-kernel-32x' }
Set >> _keysAndValuesDo: aBlock [
  "aBlock should be a 2 argument block with arguments  element, count "

  dict keysAndValuesDo:[ :each :aVal | aBlock value: each value: 1 ]
]

{ #category : '*rowan-gemstone-kernel-32x' }
Set >> - aBagOrSet [

"Difference. The result containing exactly those elements of
 the receiver that have a greater number of occurrences in the receiver than in
 the argument.
 If argument is a kind of IdentityBag, result will be an IdentitySet ,
 otherwise result is an instance of the class of the receiver."

 | res |
 (aBagOrSet isKindOf: IdentityBag ) ifTrue:[ | s |
   s := IdentitySet new .
   dict keysAndValuesDo:[ :each :aVal | s add: each ] .
   ^ s - aBagOrSet 
 ].
 ((aBagOrSet isKindOf: Bag) or:[ aBagOrSet isKindOf: Set ]) ifFalse:[
   aBagOrSet _validateKindOfClasses: { Bag . Set } .
 ].
 res := self copy .
 aBagOrSet _keysAndValuesDo:[ :each :count | | oldVal |
   oldVal := dict at: each otherwise: nil .
   oldVal ifNotNil:[ res remove: each ifAbsent: nil ].
 ] .
 ^ res
]
