smalltalk api
gitPresentIn: gitRepoPath

	| gitHome command cdResponse |
	[ 
	gitHome := self gitrevparseShowTopLevelIn: gitRepoPath.
	command := 'set -e; cd ' , gitRepoPath , '; pwd'.
	cdResponse := self performOnServer: command logging: true ]
		on: Error
		do: [ :ex | ^ false ].
	^ (self readlink: gitHome) = (self readlink: cdResponse)
