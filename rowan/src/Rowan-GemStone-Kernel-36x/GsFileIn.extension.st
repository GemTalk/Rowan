Extension { #name : 'GsFileIn' }

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentPackage [
	"when upgrading Rowan from older versions, the message #gemstoneTools is removed from Rowan, but
		these methods are using during filein, so cannot get an MNU, thus #respondsTo:"

	(Rowan respondsTo: #'gemstoneTools')
		ifFalse: [ ^ nil ].
	^ Rowan gemstoneTools topaz currentTopazPackageName
]

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentPackage: aStringOrNil [
	"when upgrading Rowan from older versions, the message #gemstoneTools is removed from Rowan, but
		these methods are using during filein, so cannot get an MNU, thus #respondsTo:"

	(Rowan respondsTo: #'gemstoneTools')
		ifFalse: [ ^ self ].
	Rowan gemstoneTools topaz currentTopazPackageName: aStringOrNil
]

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentProject [
	"when upgrading Rowan from older versions, the message #gemstoneTools is removed from Rowan, but
		these methods are using during filein, so cannot get an MNU, thus #respondsTo:"

	(Rowan respondsTo: #'gemstoneTools')
		ifFalse: [ ^ nil ].
	^ Rowan gemstoneTools topaz currentTopazProjectName
]

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentProject: aStringOrNil [
	"when upgrading Rowan from older versions, the message #gemstoneTools is removed from Rowan, but
		these methods are using during filein, so cannot get an MNU, thus #respondsTo:"

	(Rowan respondsTo: #'gemstoneTools')
		ifFalse: [ ^ self ].
	Rowan gemstoneTools topaz currentTopazProjectName: aStringOrNil
]
