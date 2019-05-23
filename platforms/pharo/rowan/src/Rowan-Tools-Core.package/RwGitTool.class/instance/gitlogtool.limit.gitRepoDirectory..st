smalltalk api
gitlogtool: commitish limit: limit gitRepoDirectory: gitRepoPath

	^ self
		gitlogIn: gitRepoPath
		with:
			'--date=relative --format="%h %cd %s" -' , limit printString , ' ' , commitish