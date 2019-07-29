Class {
	#name : 'RwComponentSpecification',
	#superclass : 'RwProjectSpecification',
	#instVars : [
		'projectName',
		'projectsPath',
		'defaultComponentName'
	],
	#category : 'Rowan-Components'
}

{ #category : 'visiting' }
RwComponentSpecification >> acceptVisitor: aVisitor [

	^ aVisitor visitComponentSpecification: self
]

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
RwComponentSpecification >> defaultComponentName [

	^ defaultComponentName ifNil: [ 'Core' ]
]

{ #category : 'accessing' }
RwComponentSpecification >> defaultComponentName: aString [

	defaultComponentName := aString
]

{ #category : 'accessing' }
RwComponentSpecification >> projectName [

	^ projectName ifNil: [ self specName ]
]

{ #category : 'accessing' }
RwComponentSpecification >> projectName: aString [

	projectName := aString
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
