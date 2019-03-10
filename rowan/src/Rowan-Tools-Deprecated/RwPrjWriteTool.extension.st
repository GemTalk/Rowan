Extension { #name : 'RwPrjWriteTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjWriteTool >> validate: aRwSpecification [
	self deprecated: 'RwPrjWriteTool>>validate: deprecated in Rowan 1.3.0'.
  super validate: aRwSpecification.
  self validateForWrite: aRwSpecification
]
