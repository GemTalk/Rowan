Class {
	#name : 'RwPrjCloneTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'private' }
RwPrjCloneTool >> _validateForGitRootPathForSpecification: gitRootPath gitRepoDir: gitRepoPath useSsh: useSsh ifDone: doneBlock [

	| gitTool response command cdResponse |
	gitTool := self class git.
	specification repositoryUrl
		ifNotNil: [ :repoUrlString | 
			| repoUrl |
			repoUrl := RwUrl fromString: repoUrlString.
			response := gitTool gitrevparseShowTopLevelIn: repoUrl pathString.
			command := 'set -e; cd ' , gitRepoPath , '; pwd'.
			cdResponse := gitTool performOnServer: command logging: true.
			response = cdResponse
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
	(Rowan fileUtilities directoryExists: gitRepoPath)
		ifTrue: [ 
			[ 
			"if gitRepoDir exists and it is a git repo, then make sure that the clone is for the target
		project ... if not error out of here."
			response := gitTool gitrevparseShowTopLevelIn: gitRepoPath.
			command := 'set -e; cd ' , gitRepoPath , '; pwd'.
			cdResponse := gitTool performOnServer: command logging: true.
			response = cdResponse
				ifTrue: [ 
					| msg |
					msg := 'A clone for ' , specification specName printString
						, ' already exists in ' , gitRepoPath printString
						, '. Updating spec repositoryUrl and skipping clone.'.
					self inform: msg.
					specification
						repositoryUrl:
							'cypress:' , gitRepoPath , '/' , specification repoPath , '/'.
					specification repositoryRootPath: gitRepoPath.
					doneBlock value: msg ] ]
				on: Error
				do: [ :ignored | 
					self
						error:
							'Expected a git repository to present in the directory: '
								, gitRepoPath printString ] ].
	(Rowan fileUtilities directoryExists: gitRootPath)
		ifFalse: [ Rowan fileUtilities ensureDirectoryExists: gitRootPath ]
]

{ #category : 'smalltalk api' }
RwPrjCloneTool >> cloneSpecification: aRwSpecification gitRootPath: gitRootPath useSsh: useSsh [

	| gitTool response projectUrl gitRepoPath cloneUrl cloneOption checkout |
	self specification: aRwSpecification.
	projectUrl := RwUrl fromString: specification projectUrl.
	gitRepoPath := gitRootPath , '/' , projectUrl segments last.
	self
		_validateForGitRootPathForSpecification: gitRootPath
		gitRepoDir: gitRepoPath
		useSsh: useSsh
		ifDone: [ :msg | 
			"done block is invoked when it is not necessary to perform further processing. Typically,
		the clone is already present."
			^ msg ].
	gitTool := self class git.
	cloneUrl := useSsh
		ifTrue: [ 'git@' , projectUrl authority , ':' ]
		ifFalse: [ 'https://' , projectUrl authority , '/' ].
	cloneUrl := cloneUrl , (projectUrl segments at: 1) , '/'
		, (projectUrl segments at: 2) , '.git'.
	cloneOption := ' --no-checkout '.
	checkout := specification repoSpec committish.
	checkout ifNil: [ cloneOption := '' ].
	response := gitTool gitcloneIn: gitRootPath with: cloneOption , cloneUrl.
	specification
		repositoryUrl: 'cypress:' , gitRepoPath , '/' , specification repoPath , '/'.
	specification repositoryRootPath: gitRepoPath.
	checkout
		ifNotNil: [ Rowan projectTools checkout checkoutSpecification: specification ].
	^ specification
]

{ #category : 'smalltalk api' }
RwPrjCloneTool >> cloneSpecUrl: aString gitRootPath: gitRootPath useSsh: useSsh [

	^self 
		cloneSpecification: (RwSpecification fromUrl: aString) 
		gitRootPath: gitRootPath 
		useSsh: useSsh

]

{ #category : 'smalltalk api' }
RwPrjCloneTool >> specification: aRwSpecification [
  "let the validation run before sending #register"

  ^ (super specification: aRwSpecification) register
]
