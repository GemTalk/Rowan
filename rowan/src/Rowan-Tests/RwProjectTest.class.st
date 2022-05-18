Class {
	#name : 'RwProjectTest',
	#superclass : 'RwInterfaceTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests - issue 428' }
RwProjectTest >> test_issue428_loaded_no_disk [

| projectName  packageName projectDefinition projectSetDefinition |

	projectName := 'Issue428'.
	packageName := 'Issue428-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
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

{ #category : 'tests - issue 428' }
RwProjectTest >> test_issue428_loaded_on_disk [

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
				root: '/tmp/rowanSimpleProject_01/' ].

"test existsOnDisk"

	self assert: (RwProject newNamed: projectName) existsOnDisk.
]

{ #category : 'tests' }
RwProjectTest >> testProjectCreation [

	| project |

	project := RwProject newNamed: 'ProjectInterface'.

]
