Extension { #name : 'GsFileIn' }

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentPackage [

	^ Rowan gemstoneTools topaz currentTopazPackageName

]

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentPackage: aStringOrNil [

	Rowan gemstoneTools topaz currentTopazPackageName: aStringOrNil

]

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentProject [

	^ Rowan gemstoneTools topaz currentTopazProjectName

]

{ #category : '*rowan-gemstone-kernel-36x' }
GsFileIn >> currentProject: aStringOrNil [

	Rowan gemstoneTools topaz currentTopazProjectName: aStringOrNil

]
