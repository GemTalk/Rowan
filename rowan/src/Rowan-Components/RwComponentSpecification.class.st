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
	^RwComponentProjectDefinition newForSpecification: self
]

{ #category : 'converting' }
RwComponentSpecification >> asProjectReferenceDefinition [

	"answer an RwProjectReferenceDefinition created using the reciever"
	^RwProjectReferenceDefinition newForSpecification: self
]

{ #category : 'accessing' }
RwComponentSpecification >> projectsPath [

	^ projectsPath ifNil: [ 'rowan/projects' ]
]

{ #category : 'accessing' }
RwComponentSpecification >> projectsPath: aString [ 

	projectsPath := aString
]

{ #category : 'accessing' }
RwComponentSpecification >> version [

	"return an RwSemanticVersionNumber"

	^ self repoSpec version
]
