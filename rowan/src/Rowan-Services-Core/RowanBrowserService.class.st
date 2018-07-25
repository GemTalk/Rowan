"
RowanBrowserService handles services that fall outside
the scope of other services.
"
Class {
	#name : 'RowanBrowserService',
	#superclass : 'RowanService',
	#instVars : [
		'projects'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'updates' }
RowanBrowserService >> update [
	| sortedProjects | 
	sortedProjects := SortedCollection sortBlock: [:a :b | a name < b name]. 
	sortedProjects addAll:  Rowan image loadedProjects.
	projects := sortedProjects collect:[:project | RowanProjectService newNamed: project name].
	RowanCommandResult addResult: self
]
