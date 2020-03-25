Class {
	#name : 'RwGemStoneTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-GemStone'
}

{ #category : 'repository' }
RwGemStoneTool >> newRepositoryRoot: repositoryRoot forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk"

	| project |
	project := Rowan projectNamed: projectName.
	^ project repositoryRoot: repositoryRoot
]
