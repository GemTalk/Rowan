Class {
	#name : 'RwAbstractProjectReferenceDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
		forSpecification: aRwSpecification 
		projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self
		projectUrl: aRwSpecification projectUrl
		projectName: projectName
		projectAlias: nil
		configurationNames: aRwSpecification defaultConfigurationNames
		groupNames: aRwSpecification defaultGroupNames
]

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> projectUrl: anUrlString projectName: aProjectName projectAlias: aProjectAlias configurationNames: configurationNames groupNames: groupNames [


	^ self new
		projectUrl: anUrlString;
		projectName: aProjectName;
		projectAlias: aProjectAlias;
		configurationNames: configurationNames;
		groupNames: groupNames;
		yourself
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> configurationNames [

	^ self properties at: 'configurationNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> configurationNames: anArray [

	^ self properties at: 'configurationNames' put: anArray
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> groupNames [

	^ self properties at: 'groupNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> groupNames: anArray [

	^ self properties at: 'groupNames' put: anArray
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> key [

	^ self projectName
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectAlias [

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectAlias: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [] ].
	^ self properties at: 'projectAlias' put: aStringOrNil
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectHome [

	^ self properties at: 'projectHome'
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectHome: projectHomeFileReferenceOrString [

	^ self properties at: 'projectHome' put: projectHomeFileReferenceOrString
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectName [

	^ self properties at: 'projectName' ifAbsent: []
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectName: aString [

	^ self properties at: 'projectName' put: aString
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectUrl [

	^ self properties at: 'projectUrl'
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectUrl: anUrlString [

	self properties at: 'projectUrl' put: anUrlString
]

{ #category : 'actions' }
RwAbstractProjectReferenceDefinition >> resolve [

	^ self resolve: FileLocator rowanProjectsHome
]

{ #category : 'actions' }
RwAbstractProjectReferenceDefinition >> resolve: rowanProjectsHome [

self halt.
]
