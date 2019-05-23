tests - issue 428
test_issue428_loaded_on_disk

	| projectName projectDefinition projectTools packageNames |
	projectName := 'Issue428'.
	packageNames := #('Issue428-Core' 'Issue428-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		handleConfirmationDuring: [ 
			projectDefinition := projectTools create
				createGitBasedProject: projectName
				packageNames: packageNames
				format: 'tonel'
				root: '/tmp/rowanSimpleProject/' ].

"test existsOnDisk"

	self assert: (RwProject newNamed: projectName) existsOnDisk.