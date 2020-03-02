Extension { #name : 'RwProjectTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> test_issue428_loaded_no_disk [

| projectName  packageName projectDefinition projectSetDefinition |

	projectName := 'Issue428'.
	packageName := 'Issue428-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := ((Rowan globalNamed: 'RwComponentProjectDefinition')
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"test existsOnDisk"

	self deny: (RwProject newNamed: projectName) existsOnDisk.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> test_issue428_loaded_on_disk [

	| projectName projectDefinition projectTools packageNames |
	projectName := 'Issue428'.
	packageNames := #('Issue428-Core' 'Issue428-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := ((Rowan globalNamed: 'RwComponentProjectDefinition')
		newForGitBasedProjectNamed: projectName)
		addPackagesNamed: packageNames;
		packageFormat: 'tonel';
		projectHome: self _testRowanProjectsSandbox pathString, '/rowanSimpleProject/';
		yourself.

	self
		handleConfirmationDuring: [ projectDefinition create ].

"test existsOnDisk"

	self assert: (RwProject newNamed: projectName) existsOnDisk.
]
