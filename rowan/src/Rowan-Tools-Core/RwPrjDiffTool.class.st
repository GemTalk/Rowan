Class {
	#name : 'RwPrjDiffTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjDiffTool >> diffSpecUrl: aString [

	^ self diffSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'validation' }
RwPrjDiffTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
