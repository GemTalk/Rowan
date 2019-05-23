smalltalk api
gitlogFull: commitish limit: limit gitRepoDirectory: gitRepoPath

	| commitLog |
	commitLog := self
		gitlogIn: gitRepoPath
		with: ' -' , limit printString , ' ' , commitish.
	^ commitLog