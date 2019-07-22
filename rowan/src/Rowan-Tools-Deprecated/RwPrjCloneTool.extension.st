Extension { #name : 'RwPrjCloneTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjCloneTool >> _validateForGitRootPathForSpecification: gitRootPath gitRepoDir: gitRepoPath useSsh: useSsh ifDone: doneBlock [

	| gitTool response command cdResponse |
self deprecated: 'Use RwPrjCloneTool>>cloneRepository:'.
	gitTool := Rowan gitTools.
	specification repositoryUrl
		ifNotNil: [ :repoUrlString | 
			| repoUrl |
			repoUrl := RwUrl fromString: repoUrlString.
			response := gitTool gitrevparseShowTopLevelIn: repoUrl pathString.
			command := 'set -e; cd ' , gitRepoPath , '; pwd'.
			cdResponse := gitTool performOnServer: command logging: true.
			(self readlink: response) = (self readlink: cdResponse)
				ifTrue: [ 
					| msg |
					msg := 'A clone for ' , specification specName printString
						, ' already exists in ' , gitRepoPath printString , '. Skipping clone.'.
					self inform: msg.
					doneBlock value: msg ].
			self
				error:
					'A clone for a different project (' , response printString
						, ') is already present in ' , gitRepoPath printString ].
	gitRepoPath exists
		ifTrue: [ 
			[ 
			"if gitRepoDir exists and it is a git repo, then make sure that the clone is for the target
		project ... if not error out of here."
			response := gitTool gitrevparseShowTopLevelIn: gitRepoPath pathString.
			command := 'set -e; cd ' , gitRepoPath pathString , '; pwd'.
			cdResponse := gitTool performOnServer: command logging: true.
			(self readlink: response) = (self readlink: cdResponse)
				ifTrue: [ 
					| msg |
					specification
						repositoryUrl:
							'cypress:' , gitRepoPath pathString , '/' , specification repoPath , '/'.
					specification repositoryRootPath: gitRepoPath pathString.
					msg := 'A clone for '
						, specification specName printString , ' already exists in '
						, gitRepoPath printString , ', so the clone operation is being skipped'
						, '
The project project has been registered with Rowan at the existing location .'.
					self inform: msg.
					doneBlock value: msg ] ]
				on: Error
				do: [ :ignored | 
					self
						error:
							'Expected a git repository to present in the directory: '
								, gitRepoPath pathString printString ] ].
	gitRootPath ensureCreateDirectory
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCloneTool >> cloneSpecification: aRwSpecification gitRootPath: gitRootPath useSsh: useSsh [

self deprecated: 'Use RwPrjCloneTool>>cloneRepository:'.
	^ self cloneSpecification: aRwSpecification gitRootPath: gitRootPath useSsh: useSsh registerProject: true
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCloneTool >> cloneSpecification: aRwSpecification gitRootPath: gitRootPathOrString useSsh: useSsh registerProject: registerProject [

	| gitTool response projectUrl gitRepoPath cloneUrl cloneOption checkout segments gitRootPath |
self deprecated: 'Use RwPrjCloneTool>>cloneRepository:'.
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
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCloneTool >> cloneSpecUrl: aString gitRootPath: gitRootPath useSsh: useSsh [

self deprecated: 'Use RwPrjCloneTool>>cloneRepository:'.
	^self 
		cloneSpecification: (RwSpecification fromUrl: aString) 
		gitRootPath: gitRootPath 
		useSsh: useSsh
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCloneTool >> specification: aRwSpecification [
  "let the validation run before sending #register"

self deprecated: 'Use RwPrjCloneTool>>cloneRepository:'.
  ^ (super specification: aRwSpecification) register
]
