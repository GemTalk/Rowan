Class {
	#name : 'RwPrjRegistryTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjRegistryTool >> registeredSpecNames [

	^ Rowan projectNames
]

{ #category : 'smalltalk api' }
RwPrjRegistryTool >> registeredSpecs [

	^ Rowan projectNames collect: [ :aString | Rowan image loadedProjectNamed: aString ]

]
