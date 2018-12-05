Extension { #name : 'RwPrjLoadTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> _markProjectSetNotDirty: projectSetDefinition [

	self deprecated: 'Use public melthod RwPrjLoadTool>>markProjectSetNotDirty: instead'.

	self markProjectSetNotDirty: projectSetDefinition

]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectNamed_old: projectNamed [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	self deprecated: 'RwPrjLoadTool >> loadProjectNamed_old: deprecated in Rowan 1.2.6+'.
	^ self loadProjectNamed_old: projectNamed instanceMigrator: Rowan platform instanceMigrator
]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectNamed_old: projectNamed instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	self deprecated: 'RwPrjLoadTool >> loadProjectNamed_old:instanceMigrator: deprecated in Rowan 1.2.6+'.
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed.
	res := self 
		_doProjectSetLoad: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	self deprecated: 'RwPrjLoadTool >> loadProjectNamed:withConfiguration: deprecated in Rowan 1.2.6+'.
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfiguration: configName.
	[ res := self loadProjectSetDefinition: projectSetDefinition  ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].
	"loaded project and loaded packages read from disk - mark them not dirty"
	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].
	^ res
]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	self deprecated: 'RwPrjLoadTool >> loadProjectNamed:withConfiguration:instanceMigrator: deprecated in Rowan 1.2.6+'.
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfiguration: configName.
	[ res := self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].
	"loaded project and loaded packages read from disk - mark them not dirty"
	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].
	^ res
]
