Class {
	#name : 'RowanComponentService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'componentServices',
		'packageServices',
		'projectService',
		'basename'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanComponentService class >> forComponentNamed: componentName projectService: theProjectService [
	| inst component |
	component := theProjectService rwProject componentNamed: componentName.
	inst := self new
		name: componentName;
		basename: component basename.
	inst computeSubComponentsUsingProjectService: theProjectService.
	inst projectService: theProjectService. 
	^ inst
]

{ #category : 'operations' }
RowanComponentService >> addFlattenedHierarchyTo: dictionary [
	dictionary
		at: self
		put: (componentServices collect: [ :service | service ]).
	componentServices
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
]

{ #category : 'accessing' }
RowanComponentService >> basename [
	^basename
]

{ #category : 'accessing' }
RowanComponentService >> basename: object [
	basename := object
]

{ #category : 'accessing' }
RowanComponentService >> component [
	^ projectService rwProject componentNamed: name
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
RowanComponentService >> computeSubComponentsUsingProjectService: theProjectService [
	componentServices := (theProjectService rwProject
		subcomponentsOf: name
		ifNone: [ Array new ])
		collect: [ :subcomponent | 
			RowanComponentService
				forComponentNamed: subcomponent name
				projectService: theProjectService ]
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
RowanComponentService >> projectService [
	^projectService
]

{ #category : 'accessing' }
RowanComponentService >> projectService: theProjectService [
	projectService := theProjectService
]

{ #category : 'accessing' }
RowanComponentService >> subcomponentsOf: rwProject [
]

{ #category : 'updating' }
RowanComponentService >> update [
	super update.
	self updatePackageServices
]

{ #category : 'clientCommands' }
RowanComponentService >> updatePackageServices [
	packageServices := (projectService rwProject allPackageNamesIn: name)
		collect: [ :packageName | RowanPackageService forPackageNamed: packageName ].
	RowanCommandResult addResult: self
]
