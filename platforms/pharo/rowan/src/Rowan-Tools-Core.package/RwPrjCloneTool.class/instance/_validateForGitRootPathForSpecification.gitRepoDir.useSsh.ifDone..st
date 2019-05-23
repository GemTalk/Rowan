private
_validateForGitRootPathForSpecification: gitRootPath gitRepoDir: gitRepoPath useSsh: useSsh ifDone: doneBlock

	| gitTool response command cdResponse |
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