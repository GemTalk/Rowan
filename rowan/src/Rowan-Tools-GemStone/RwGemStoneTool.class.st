Class {
	#name : 'RwGemStoneTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-GemStone'
}

{ #category : 'repository' }
RwGemStoneTool >> newRepositoryRoot: repositoryRoot forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk"

	| resolvedProject project |
	project := Rowan projectNamed: projectName.
	resolvedProject := project asDefinition.
	resolvedProject repositoryRoot: repositoryRoot.
	^ resolvedProject loadProjectSet
]

{ #category : 'repository' }
RwGemStoneTool >> newRepositoryRootForRowan: repositoryRoot [
	"Rowan has several embedded projects and that means for the moment that the repsitoryRoot for the 
		projects must be changed at the same time, then load Rowan which will load the embedded projects"

	"https://github.com/GemTalk/Rowan/issues/591"

	| resolvedProject project |
	#('Cypress' 'FileSystemGs' 'Rowan' 'STON' 'Tonel')
		do: [ :projectName | 
			project := RwProject newNamed: projectName.
			project repositoryRoot: repositoryRoot ].

	resolvedProject := (Rowan image loadedProjectNamed: 'Rowan') asDefinition.
	resolvedProject loadProjectSet
]
