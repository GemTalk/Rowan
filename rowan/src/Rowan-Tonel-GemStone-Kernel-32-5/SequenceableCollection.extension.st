Extension { #name : 'SequenceableCollection' }

{ #category : '*rowan-tonel-gemstone-kernel-32-5' }
SequenceableCollection >> second [

	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2

]
