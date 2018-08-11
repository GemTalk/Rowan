"
RowanBrowserService handles services that fall outside
the scope of other services.
"
Class {
	#name : 'RowanBrowserService',
	#superclass : 'RowanService',
	#instVars : [
		'projects',
		'removedMethods',
		'allClasses',
		'hierarchyServices'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'commands' }
RowanBrowserService >> allClasses [

	allClasses := SortedCollection sortBlock: [:x :y | x name < y name].
	allClasses addAll: (organizer classes collect:[:class | 
			| service |
			service := RowanClassService new name: class name. 
			service packageName: class rowanPackageName.
			service projectName: class rowanProjectName.
			service]
			).
	allClasses := allClasses asArray. 
	updateType := #classes. "#classes not used at the moment so no updates will be done"
	RowanCommandResult addResult: self
]

{ #category : 'commands' }
RowanBrowserService >> defaultClassHierarchy [
	hierarchyServices := Dictionary new.   
	organizer hierarchy keysAndValuesDo: [:key :value |
		| classService |
		classService := key == #nil ifTrue:[#nil] ifFalse: [RowanClassService basicForClassNamed: key name].
		hierarchyServices at: classService put: (value collect:[:cls | RowanClassService basicForClassNamed: cls name]) asArray.
	].
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
]

{ #category : 'commands' }
RowanBrowserService >> removeMethods: methodServices [

	| notRemoved |
	notRemoved := Array new. 
	removedMethods := Array new. 
	methodServices do: [:methodService |
		| classService |
		classService := RowanClassService forClassNamed: methodService className. 
		classService meta: methodService meta. 
		classService removeSelector: methodService selector ifAbsent:[notRemoved add: methodService selector].
		removedMethods add: methodService].
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', notRemoved printString].
	updateType := #methodsRemoved:. 
	RowanCommandResult addResult: self.
]

{ #category : 'update' }
RowanBrowserService >> update [

	self updateProjects
]

{ #category : 'commands' }
RowanBrowserService >> updateProjects [
	| sortedProjects | 
	sortedProjects := SortedCollection sortBlock: [:a :b | a name < b name]. 
	sortedProjects addAll:  Rowan image loadedProjects.
	projects := sortedProjects collect:[:project | RowanProjectService newNamed: project name].
	updateType := #projectsUpdate:.
	RowanCommandResult addResult: self

]
