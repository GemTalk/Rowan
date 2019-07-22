Extension { #name : 'RwProjectTool' }

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> spec [

	self deprecated: 'Use exportSpecification message instead'.
  ^ RwPrjSpecTool new
]
