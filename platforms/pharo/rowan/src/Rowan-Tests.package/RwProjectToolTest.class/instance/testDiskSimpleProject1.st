tests
testDiskSimpleProject1

	"Create project and build disk-based artifacts first, then create create a class and write changes to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition packageNames loadedProject |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		handleConfirmationDuring: [ 
			projectDefinition := projectTools create
				createDiskBasedProject: projectName
				packageNames: packageNames
				format: 'tonel'
				root: '/tmp/rowanSimpleProject/'].
	projectDefinition
		comment:
				'This is a simple project to demonstrate the smalltalk API used for a project lifecycle';
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectTools commit
		commitProjectDefinition: projectDefinition
		message: 'Initial commit'.
	self
		handleInformAsFailureDuring: [ projectTools load loadProjectDefinition: projectDefinition ].

	loadedProject := Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self assert: false description: 'expected to find loaded project' ].
	packageNames
		do: [ :packageName | 
			"ensure that we have a loaded package for each of the packages"
			Rowan image
				loadedPackageNamed: packageName
				ifAbsent: [ self assert: false description: 'expected to find loaded package' ] ].

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple class'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	self
		handleInformAsFailureDuring: [ projectTools load loadProjectDefinition: projectDefinition ].

	projectTools spec exportSpecification: projectDefinition specification.
	projectTools write writeProjectDefinition: projectDefinition.
	projectTools commit
		commitProjectDefinition: projectDefinition
		message: 'Added Simple class'
