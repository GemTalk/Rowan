smalltalk api
cloneSpecification: aRwSpecification gitRootPath: gitRootPathOrString useSsh: useSsh registerProject: registerProject

	| gitTool response projectUrl gitRepoPath cloneUrl cloneOption checkout segments gitRootPath |
	registerProject
		ifTrue: [ 
			"Register project show that is shows up as a loaded project in project list ... ready to be explicitly loaded"
			self specification: aRwSpecification ]
		ifFalse: [ specification := aRwSpecification ].
	gitRootPath := gitRootPathOrString asFileReference.
	gitRepoPath := gitRootPath / aRwSpecification specName.
	self
		_validateForGitRootPathForSpecification: gitRootPath
		gitRepoDir: gitRepoPath
		useSsh: useSsh
		ifDone: [ :msg | 
			"done block is invoked when it is not necessary to perform further processing. Typically,
		the clone is already present."
			^ msg ].
	gitTool := Rowan gitTools.
	projectUrl := RwUrl fromString: specification projectUrl.
	cloneUrl := useSsh
		ifTrue: [ 'git@' , projectUrl authority , ':' ]
		ifFalse: [ 'https://' , projectUrl authority , '/' ].
	segments := projectUrl segments.
	cloneUrl := segments size = 1
		ifTrue: [ cloneUrl , (segments at: 1) , '.git' ]
		ifFalse: [ cloneUrl , (segments at: 1) , '/' , (projectUrl segments at: 2) , '.git' ].
	cloneOption := ' --no-checkout '.
	checkout := specification repoSpec committish.
	checkout ifNil: [ cloneOption := '' ].
	response := gitTool gitcloneIn: gitRootPath  pathString with: cloneOption , cloneUrl.
	specification
		repositoryUrl: 'cypress:' , gitRepoPath pathString  , '/' , specification repoPath , '/'.
	specification repositoryRootPath: gitRepoPath.
	checkout
		ifNotNil: [ Rowan projectTools checkout checkoutSpecification: specification ].
	^ specification