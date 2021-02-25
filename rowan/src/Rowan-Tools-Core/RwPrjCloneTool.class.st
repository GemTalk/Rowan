Class {
	#name : 'RwPrjCloneTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjCloneTool >> _validateForGitRepository: aRwGitRepositoryDefinition ifDone: doneBlock [

	| gitTool response command cdResponse gitRepoPath |
	gitTool := Rowan gitTools.
	gitRepoPath := aRwGitRepositoryDefinition repositoryRoot.

	gitRepoPath exists
		ifTrue: [ 
			[ 
			"if gitRepoDir exists and it is a git repo, then make sure that the clone is for the target
		project ... if not error out of here."
			response := gitTool gitrevparseShowTopLevelIn: gitRepoPath pathString.
			command := 'set -e; cd ' , gitRepoPath pathString , '; pwd'.
			cdResponse := gitTool performOnServer: command.
			(self readlink: response) = (self readlink: cdResponse)
				ifTrue: [ 
					| msg |
					msg := 'A clone for '
						, aRwGitRepositoryDefinition name printString , ' already exists in '
						, gitRepoPath pathString printString , ', so the clone operation is being skipped'
						, '
The project project has been registered with Rowan at the existing location .'.
					doneBlock value: msg ] 
				ifFalse: [ self error: 'expected repository path does not match gitrevparse result.' ]]
				on: Error
				do: [ :ignored | 
					self
						error:
							'Expected a git repository to present in the directory: '
								, gitRepoPath pathString printString ] ].
	gitRepoPath parent ensureCreateDirectory
]

{ #category : 'smalltalk api' }
RwPrjCloneTool >> cloneRepository: aRwGitRepositoryDefinition [
	| gitTool response remoteUrl cloneOption checkout |
	self
		_validateForGitRepository: aRwGitRepositoryDefinition
		ifDone: [ :msg | 
			"done block is invoked when it is not necessary to perform further processing. Typically,
		the clone is already present."
			^ msg ].
	gitTool := Rowan gitTools.
	remoteUrl := aRwGitRepositoryDefinition remoteUrl , ' '.
	cloneOption := ' --no-checkout '.
	checkout := aRwGitRepositoryDefinition committish.
	checkout ifNil: [ cloneOption := '' ].
	response := gitTool
		gitcloneIn: aRwGitRepositoryDefinition repositoryRoot parent pathString
		with:
			cloneOption , remoteUrl , aRwGitRepositoryDefinition repositoryRoot pathString.
	checkout
		ifNotNil: [ 
			gitTool := Rowan gitTools.
			gitTool
				gitcheckoutIn: aRwGitRepositoryDefinition repositoryRoot
				with: checkout ]
]
