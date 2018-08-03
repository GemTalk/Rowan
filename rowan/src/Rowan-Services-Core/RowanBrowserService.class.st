"
RowanBrowserService handles services that fall outside
the scope of other services.
"
Class {
	#name : 'RowanBrowserService',
	#superclass : 'RowanService',
	#instVars : [
		'projects',
		'removedMethods'
	],
	#category : 'Rowan-Services-Core'
}

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

{ #category : 'commands' }
RowanBrowserService >> updateProjects [
	| sortedProjects | 
	sortedProjects := SortedCollection sortBlock: [:a :b | a name < b name]. 
	sortedProjects addAll:  Rowan image loadedProjects.
	projects := sortedProjects collect:[:project | RowanProjectService newNamed: project name].
	updateType := #projectsUpdate:.
	RowanCommandResult addResult: self

]
