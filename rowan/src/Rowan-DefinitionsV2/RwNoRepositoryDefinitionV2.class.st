Class {
	#name : 'RwNoRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'testing' }
RwNoRepositoryDefinitionV2 >> repositoryExists [
	^ false
]

{ #category : 'accessing' }
RwNoRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, specsPath, and projectsPath are specified relative to the repository root."

	^ nil
]

{ #category : 'actions' }
RwNoRepositoryDefinitionV2 >> resolve [
	"nothing on disk"

	^ false
]
