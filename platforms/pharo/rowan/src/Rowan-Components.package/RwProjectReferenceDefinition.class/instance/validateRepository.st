actions
validateRepository
	"cofirm that the repository appears to be complete relative to this project"

	{
		'configs' -> self configsRoot. 
		'packages' -> self packagesRoot. 
		'specs' -> self specsRoot. 
		'projects' -> self projectsRoot
	}
		do: [ :assoc | 
			(assoc value) exists 
				ifFalse: [ self error: 'The repository at ', self repositoryRootPath printString, ' is missing the ', (assoc key) printString, ' directory.'  ] ].