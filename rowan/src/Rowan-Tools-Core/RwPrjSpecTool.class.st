Class {
	#name : 'RwPrjSpecTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportProjectDefinition: projectDefinition [

	self specification: projectDefinition specification.
	^ specification export
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportProjectNamed: projectName [

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	self specification: loadedProject specification.
	^ specification export
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecification: aRwSpecification [

	self specification: aRwSpecification.
	^specification export
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecification: aRwSpecification toUrl: fileUrl [

	self specification: aRwSpecification.
	^specification exportToUrl: fileUrl

]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecUrl: aString [

	^ self exportSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecUrl: aString toUrl: exportUrl [

	^ self exportSpecification: (RwSpecification fromUrl: aString) toUrl: exportUrl
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> specForProjectNamed: projectName [

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	^ loadedProject specification
]
