loaded project creation
createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the git project exists on disk
		it will be used, if the git project does not exist on disk it will be created. "

	| projectDefinition |
	projectDefinition := (RwProjectDefinition newForGitBasedProjectNamed: projectName)
		packageNames: packageNames;
		yourself.
	^ self
		createProjectFor: projectDefinition 
		format: format 
		root: rootPath 
		configsPath: 'rowan/configs' 
		repoPath: 'rowan/src' 
		specsPath: 'rowan/specs'
