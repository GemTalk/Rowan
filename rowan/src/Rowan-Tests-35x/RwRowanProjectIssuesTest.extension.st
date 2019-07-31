Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-tests-35x' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName packageNames: packageNames root: rootPath symbolDictionaryName: symbolDictionaryName validate: validate [

	| projectDefinition project |

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := (self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symbolDictionaryName
		comment: 'Basic project ', projectName printString)
			packageFormat: 'tonel';
			projectHome: rootPath; 
			configsPath: 'configs';
			packagesPath: 'src';
			projectsPath: 'src';
			specsPath: 'specs';
			yourself.

	self
		handleConfirmationDuring: [
			projectDefinition create ].

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

]
