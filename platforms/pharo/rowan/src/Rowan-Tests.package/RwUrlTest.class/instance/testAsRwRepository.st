tests
testAsRwRepository

	| repos loadSpec repoRoot testRepoPath loadedProject |
	loadedProject := Rowan image _projectForNonTestProject: 'Rowan'.
	loadSpec := loadedProject specification.
	repoRoot := loadSpec repoSpec repositoryRootPath.
	testRepoPath := repoRoot , '/test/testRepositories/repos'.
	repos := {('cypress:' , testRepoPath , '/cypress/').
	('filetree://' , testRepoPath , '/filetree/').
	('tonel:' , testRepoPath , '/tonel/')}
		collect: [ :urlString | urlString -> urlString asRwRepository ].
	repos
		do: [ :assoc | 
			| repo x y |
			repo := assoc value.
			self assert: (x := repo url) = (y := assoc key) ]