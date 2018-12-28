Extension { #name : 'RwPrjReadTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjReadTool >> validate: aRwSpecification [
	self deprecated: 'RwPrjReadTool>>validate: deprecated in Rowan 1.3.0'.
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
