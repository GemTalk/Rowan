private-issue 24
_createLoadedProjectNamed: projectName packageNames: packageNames root: rootPath symbolDictionaryName: symbolDictionaryName validate: validate

	| projectDefinition project |

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symbolDictionaryName
		comment:
			'Basic project ', projectName printString.

	self
		handleConfirmationDuring: [
			Rowan projectTools create 
				createProjectFor: projectDefinition 
				format: 'tonel' 
				root:rootPath 
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"
