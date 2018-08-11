Class {
	#name : 'RowanProjectsUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'projects',
		'modifiedProjects',
		'selection'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'initialize' }
RowanProjectsUpdate >> initialize [

	projects := Array new. 
	modifiedProjects := Array new.
	selection := Array new.

]

{ #category : 'accessing' }
RowanProjectsUpdate >> modifiedProjects [

	^modifiedProjects

]

{ #category : 'accessing' }
RowanProjectsUpdate >> projects [

	^projects

]

{ #category : 'accessing' }
RowanProjectsUpdate >> selection [

	^selection

]
