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
	^ nil
]

{ #category : 'accessing' }
RwNoRepositoryDefinitionV2 >> repositoryRoot: ignored [
]

{ #category : 'actions' }
RwNoRepositoryDefinitionV2 >> resolve [
	"nothing on disk"

	^ false
]
