Extension { #name : 'SequenceableCollection' }

{ #category : '*rowan-gemstone-kernel-32' }
SequenceableCollection >> indexOfSubCollection: aSubColl [

"Returns the index of the first element of the receiver where that element and
 the subsequent ones are equal to those in aSubColl. The search is begun in the
 receiver starting at 1."

^ self indexOfSubCollection: aSubColl startingAt: 1.
]
