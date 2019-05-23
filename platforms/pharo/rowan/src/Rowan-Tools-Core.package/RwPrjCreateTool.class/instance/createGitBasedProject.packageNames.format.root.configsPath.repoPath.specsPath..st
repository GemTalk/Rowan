loaded project creation
createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the git project exists on disk
		it will be used, if the git project does not exist on disk it will be created. "

	| projectDefinition |
	projectDefinition := self 
		createGitBasedProjectDefinition: projectName 
		packageNames: packageNames 
		format: format 
		root: rootPath 
		configsPath: configsPath 
		repoPath: repoPath 
		specsPath: specsPath.
	^ self
		createProjectFor: projectDefinition 
		format: format
