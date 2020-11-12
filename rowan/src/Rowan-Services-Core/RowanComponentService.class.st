Class {
	#name : 'RowanComponentService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'componentServices',
		'packageServices',
		'projectService'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanComponentService class >> forComponentNamed: componentName project: rwProject [
	| inst |
	inst := self new name: componentName.
	inst computeSubComponentsUsing: rwProject.
	^ inst
]

{ #category : 'operations' }
RowanComponentService >> addFlattenedHierarchyTo: dictionary [
	dictionary
		at: name
		put: (componentServices collect: [ :service | service name ]).
	componentServices
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
]

{ #category : 'accessing' }
RowanComponentService >> componentServices [
	^componentServices
]

{ #category : 'accessing' }
RowanComponentService >> componentServices: anArray [

	componentServices := anArray
]

{ #category : 'operations' }
RowanComponentService >> computeSubComponentsUsing: rwProject [
	componentServices := ([ rwProject subcomponentsOf: name ]
		on: Error
		do: [ :ex | Array new	"until ifNone: block is written" ])
		collect: [ :subcomponent | RowanComponentService forComponentNamed: subcomponent name project: rwProject ]
]

{ #category : 'initialization' }
RowanComponentService >> initialize [
	" to do "
	super initialize.
	componentServices := Array new.
]

{ #category : 'accessing' }
RowanComponentService >> name [
	^name
]

{ #category : 'accessing' }
RowanComponentService >> name: object [
	name := object
]

{ #category : 'accessing' }
RowanComponentService >> subcomponents [
]

{ #category : 'accessing' }
RowanComponentService >> subcomponentsOf: rwProject [
]
