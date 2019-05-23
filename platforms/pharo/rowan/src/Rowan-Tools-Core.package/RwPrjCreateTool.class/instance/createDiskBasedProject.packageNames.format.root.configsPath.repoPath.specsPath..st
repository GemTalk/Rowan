loaded project creation
createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the project exists on disk
		it will be used, if the project does not exist on disk it will be created. "

	| projectDefinition |
	projectDefinition := self 
		createDiskBasedProjectDefinition: projectName 
		packageNames: packageNames 
		format: format 
		root: rootPath 
		configsPath: configsPath 
		repoPath: repoPath 
		specsPath: specsPath.
	^ self
		createProjectFor: projectDefinition 
		format: format
