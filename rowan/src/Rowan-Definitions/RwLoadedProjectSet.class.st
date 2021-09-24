Class {
	#name : 'RwLoadedProjectSet',
	#superclass : 'RwEntitySet',
	#category : 'Rowan-Definitions'
}

{ #category : 'accessing' }
RwLoadedProjectSet >> addAll: anRwProjectCollection [
	anRwProjectCollection do: [ :project | self addProject: project ].
	^ anRwProjectCollection
]

{ #category : 'accessing' }
RwLoadedProjectSet >> addProject: anRwProject [
	entities at: anRwProject projectName put: anRwProject.
	^ anRwProject
]

{ #category : 'definitions' }
RwLoadedProjectSet >> asDefinitionSet [

	self shouldNotImplement: #asDefinitionSet
]

{ #category : 'definitions' }
RwLoadedProjectSet >> asPackageDefinitionSet [
	self shouldNotImplement: #'asPackageDefinitionSet'
]

{ #category : 'definitions' }
RwLoadedProjectSet >> asProjectDefinitionSet [
	"this might make sense ... resolve/read the load specs?"

	self shouldNotImplement: #'asProjectDefinitionSet'
]

{ #category : 'accessing' }
RwLoadedProjectSet >> projectNamed: projectName [
	^ entities at: projectName
]

{ #category : 'accessing' }
RwLoadedProjectSet >> projectNames [
	^ entities keys
]
