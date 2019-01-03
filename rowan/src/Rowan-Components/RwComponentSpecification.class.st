Class {
	#name : 'RwComponentSpecification',
	#superclass : 'RwProjectSpecification',
	#instVars : [
		'projectsPath'
	],
	#category : 'Rowan-Components'
}

{ #category : 'converting' }
RwComponentSpecification >> asDefinition [

	"answer an RwComponentProjectDefinition created using the reciever"
self halt.
	^RwComponentProjectDefinition newForSpecification: self
]

{ #category : 'accessing' }
RwComponentSpecification >> version [

	"return an RwSemanticVersionNumber"

	^ self repoSpec version
]
