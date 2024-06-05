Extension { #name : 'RwTraitMove' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwTraitMove >> addMovedClassToPatchSet: aPatchSet [

	aPatchSet addClassMove: self
]
