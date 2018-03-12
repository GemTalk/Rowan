Extension { #name : 'GsNMethod' }

{ #category : '*rowan-tode' }
GsNMethod >> rowanPackageName [

	"answer the name of the package that the receiver is a member of. Answer '(NONE)' if 
		the receiver is not a member of any package"

	| loadedMethod |
	loadedMethod := Rowan image
		loadedMethodForMethod: self
		ifAbsent: [ ^ '(NONE)' ].
	^ loadedMethod loadedPackage name
]

{ #category : '*rowan-tode' }
GsNMethod >> rowanProjectName [

	"answer the name of the project that the receiver is a member of. Answer '(NONE)' if 
		the receiver is not a member of any project"

	| loadedMethod |
	loadedMethod := Rowan image
		loadedMethodForMethod: self
		ifAbsent: [ ^ '(NONE)' ].
	^ loadedMethod loadedProject name
]
