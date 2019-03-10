Class {
	#name : 'RwPrjUnregisterTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Deprecated'
}

{ #category : 'smalltalk api' }
RwPrjUnregisterTool >> unregisterSpecification: aRwSpecification [

  self specification: aRwSpecification.
  ^ specification unregister
]

{ #category : 'smalltalk api' }
RwPrjUnregisterTool >> unregisterSpecUrl: aString [

	^ self unregisterSpecification: (RwSpecification fromUrl: aString)
]
