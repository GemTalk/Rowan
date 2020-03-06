Class {
	#name : 'RwPrjCreateTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV1'
}

{ #category : 'private' }
RwPrjCreateTool >> _createProjectRepositoryDirectory: repoPathIn useGit: useGit abort: abortBlock [

	| gitTool repoPath |
	gitTool := Rowan gitTools.
	repoPath := repoPathIn asFileReference.
	repoPath exists
		ifTrue: [^ abortBlock value  ]
		ifFalse: [ repoPath ensureCreateDirectory ].
	useGit
		ifTrue: [ 
			(gitTool gitPresentIn: repoPath pathString )
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: repoPath pathString with: '' ] ]
]

{ #category : 'project repository creation' }
RwPrjCreateTool >> createProjectRepository: projectReferenceDefinition [

	"Create create new repository on `disk`, based on the given repository definition, if it does not already exist."

	| repositoryDefinition |
	repositoryDefinition := projectReferenceDefinition repositoryDefinition.
	self _createProjectRepositoryDirectory: 
		repositoryDefinition repositoryRoot 
		useGit: projectReferenceDefinition useGit 
		abort: [
			"repository already exists on disk, confirm that the repository matches the disk information and continue"
			projectReferenceDefinition validateRepository.
			^ projectReferenceDefinition ].

	{projectReferenceDefinition configsRoot. projectReferenceDefinition packagesRoot. projectReferenceDefinition specsRoot. projectReferenceDefinition projectsRoot }
		do: [ :path | path ensureCreateDirectory ].

	projectReferenceDefinition exportSpecification.
	^ projectReferenceDefinition
]
