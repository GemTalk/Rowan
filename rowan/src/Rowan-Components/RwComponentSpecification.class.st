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

	"answer an RwProjectDefintion?? created using the reciever"

	self error: 'not yet implemented'.
]

{ #category : 'accessing' }
RwComponentSpecification >> version [

	"return an RwSemanticVersionNumber"

	^ self repoSpec version
]
