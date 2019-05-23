loaded project creation
createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the project exists on disk
		it will be used, if the project does not exist on disk it will be created. "

	| projectDefinition |
	projectDefinition := (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		yourself.
	^ self
		createProjectFor: projectDefinition 
		format: format 
		root: rootPath 
		configsPath: 'rowan/configs' 
		repoPath: 'rowan/src' 
		specsPath: 'rowan/specs'
