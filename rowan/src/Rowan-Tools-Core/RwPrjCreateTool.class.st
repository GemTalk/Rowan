Class {
	#name : 'RwPrjCreateTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjCreateTool >> _createProjectRepositoryDirectory: repoPathIn useGit: useGit abort: abortBlock [

	| gitTool repoPath |
	gitTool := Rowan gitTools.
	repoPath := repoPathIn asFileReference.
	repoPath exists
		ifTrue: [ 
			(self
				confirm:
					'There is already a directory named ' , repoPath pathString printString
						, '. The contents of the directory will be deleted if you continue.')
				== true
				ifFalse: [ ^ abortBlock value ].
			repoPath ensureDeleteAllChildren ]
		ifFalse: [ repoPath ensureCreateDirectory ].
	useGit
		ifTrue: [ 
			(gitTool gitPresentIn: repoPath pathString )
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: repoPath pathString with: '' ] ]
]

{ #category : 'component project defintion creation' }
RwPrjCreateTool >> createComponentProject: componentProjectDefinition [

	"Create create new repository on `disk`, based on the given repository definition."

	self createProjectRepository: componentProjectDefinition projectRef.

	componentProjectDefinition exportProjects.
	componentProjectDefinition exportComponents.
	componentProjectDefinition exportPackages.

	componentProjectDefinition register.

	^ componentProjectDefinition
]

{ #category : 'project repository creation' }
RwPrjCreateTool >> createProjectRepository: projectReferenceDefinition [

	"Create create new repository on `disk`, based on the given repository definition."

	| repositoryDefinition |
	repositoryDefinition := projectReferenceDefinition repositoryDefinition.
	self _createProjectRepositoryDirectory: 
		repositoryDefinition repositoryRoot 
		useGit: projectReferenceDefinition useGit 
		abort: [
			"abort occurs because directory exists and user has not confirmed that the directory should be deleted"
			"cofirm that the repository appears to be complete relative to this project ... we will likely be writing new information to the repo"
			projectReferenceDefinition validateRepository.
			^ projectReferenceDefinition ].

	{projectReferenceDefinition configsRoot. projectReferenceDefinition packagesRoot. projectReferenceDefinition specsRoot. projectReferenceDefinition projectsRoot }
		do: [ :path | path ensureCreateDirectory ].

	projectReferenceDefinition exportSpecification.
	^ projectReferenceDefinition
]
