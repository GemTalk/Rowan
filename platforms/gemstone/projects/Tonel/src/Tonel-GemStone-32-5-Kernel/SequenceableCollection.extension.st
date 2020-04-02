Extension { #name : 'SequenceableCollection' }

{ #category : '*tonel-gemstone-32-5-kernel' }
SequenceableCollection >> second [

	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2

]
