Extension { #name : 'RwClassMove' }

{ #category : '*rowan-gemstone-loader-extensions' }
RwClassMove >> addMovedClassToPatchSet: aPatchSet [

	aPatchSet addClassMove: self
]
