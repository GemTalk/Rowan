Extension { #name : 'GsNMethod' }

{ #category : '*rowan-gemstone-kernel' }
GsNMethod >> isFromRowan [

	^self origin isRowanLoadedMethod
]

{ #category : '*rowan-gemstone-kernel' }
GsNMethod >> rowanPackageName [

	"answer the name of the package that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any package"

	| loadedMethod |
	loadedMethod := Rowan image
		loadedMethodForMethod: self
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedMethod loadedPackage name
]

{ #category : '*rowan-gemstone-kernel' }
GsNMethod >> rowanProjectName [

	"answer the name of the project that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any project"

	| loadedMethod |
	loadedMethod := Rowan image
		loadedMethodForMethod: self
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedMethod loadedProject name
]
