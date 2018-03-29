Class {
	#name : 'RwPrjRegisterTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjRegisterTool >> registerSpecification: aRwSpecification [

  self specification: aRwSpecification.
  ^ specification register
]

{ #category : 'smalltalk api' }
RwPrjRegisterTool >> registerSpecification: aRwSpecification as: aSpecName [
  self registerSpecification: aRwSpecification copy.

]

{ #category : 'smalltalk api' }
RwPrjRegisterTool >> registerSpecUrl: aString as: aSpecName [
  ^ self registerSpecification: (RwSpecification fromUrl: aString) copy.

]
