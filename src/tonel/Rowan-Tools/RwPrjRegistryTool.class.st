Class {
	#name : 'RwPrjRegistryTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjRegistryTool >> registeredSpecNames [

	^ Rowan projectNames
]

{ #category : 'smalltalk api' }
RwPrjRegistryTool >> registeredSpecs [

	^ Rowan projectNames collect: [ :aString | Rowan projectNamed: aString ]
]

{ #category : 'smalltalk api' }
RwPrjRegistryTool >> registry [
  ^ Rowan knownProjects
]
