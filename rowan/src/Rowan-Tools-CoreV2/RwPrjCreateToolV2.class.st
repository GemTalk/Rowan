Class {
	#name : 'RwPrjCreateToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'project repository creation' }
RwPrjCreateToolV2 >> createProjectRepository: projectDefinition [
	"Create create new repository on `disk`, based on the given project definition, if it does not already exist."

	| projectLoadSpecification |
	projectLoadSpecification := projectDefinition projectLoadSpecification.
	projectLoadSpecification repositoryDefinition
		ifNil: [ self error: 'internal error - expected the repository definition to be defined' ]
		ifNotNil: [ :repositoryDefinition | 
			repositoryDefinition repositoryRoot exists
				ifFalse: [ self error: 'internal error - expected the repository root to be created' ] ].
	{(projectDefinition componentsRoot).
	(projectDefinition packagesRoot).
	(projectDefinition doitsRoot).
	(projectDefinition specsRoot).
	(projectDefinition projectsRoot)} do: [ :path | path ensureCreateDirectory ]
]

{ #category : 'project repository creation' }
RwPrjCreateToolV2 >> createResolvedProjectRepository: resolvedRepository [
	"Create create new repository on `disk`, based on the given resolved project, if it does not already exist."

	resolvedRepository repositoryRoot exists
		ifFalse: [ self error: 'internal error - expected the repository root to be created' ].
	resolvedRepository projectRoots do: [ :path | path ensureCreateDirectory ]
]
