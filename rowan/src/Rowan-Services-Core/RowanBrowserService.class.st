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

	projects := Rowan image loadedProjects collect:[:project | RowanProjectService newNamed: project name].
	RowanCommandResult addResult: self

]
