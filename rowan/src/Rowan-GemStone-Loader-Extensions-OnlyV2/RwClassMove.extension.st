Extension { #name : 'RwClassMove' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwClassMove >> addMovedClassToPatchSet: aPatchSet [

	aPatchSet addClassMove: self
]
