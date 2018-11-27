Class {
	#name : 'RwProjectTest',
	#superclass : 'RwInterfaceTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectTest >> gsInteractionConfirmationHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
		confirmBlock: [ :interaction | interaction ok ];
		yourself
]

{ #category : 'private' }
RwProjectTest >> handleConfirmationDuring: aBlock [

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionConfirmationHandler) ]
]

{ #category : 'tests - issue 428' }
RwProjectTest >> test_issue428_loaded_no_disk [

| projectName  packageName projectDefinition projectSetDefinition audit testClass |

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

	| projectName projectDefinition projectTools classDefinition packageDefinition packageNames loadedProject |
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
]

{ #category : 'tests' }
RwProjectTest >> testProjectCreation [

	| project |

	project := RwProject newNamed: 'ProjectInterface'.

]
