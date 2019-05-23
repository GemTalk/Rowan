tests
testSimpleProject2

	"Build our project in memory without committing to disk until we've created a class, then write to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple2'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple2 class'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createProjectFor: projectDefinition
				format: 'tonel'
				root: '/tmp/rowanSimpleProject2/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectTools commit
		commitProjectDefinition: projectDefinition
		message: 'Added Simple class'