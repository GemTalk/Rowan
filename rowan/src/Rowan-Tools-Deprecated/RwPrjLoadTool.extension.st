Extension { #name : 'RwPrjLoadTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> _doProjectSetLoad: projectSetDefinition instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames [
	| copiedProjectSetDef theClassName theClass projectDef theLoadedProject loadedClass packageDef |
	[ ^ self
		_loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator ]
			on: RwExistingVisitorAddingExistingClassNotification
			do: [:ex | 
				theClassName := ex classDefinition name.
				(processedClassNames includes: theClassName) ifTrue: [ ex resume ].
				theClass := Rowan globalNamed: theClassName.
				theClass isBehavior ifFalse: [ self halt. ex pass ].
				theLoadedProject := Rowan image loadedProjectNamed: theClass rowanProjectName.
				theLoadedProject 
					ifNil: [ 
						"the loaded project should not be nil - if it is, pass the notification"
						ex pass ].
				(originalProjectSet projectNamed: theLoadedProject name ifAbsent: []) 
					ifNotNil: [
						"If the loadedProject is in the originalProjectSet, then is likely to be a class move - resume and let the chips fall where they may"
						ex resume ].
				copiedProjectSetDef := projectSetDefinition copy.
				"a project in the original project set is taking ownership of an already  loaded class,
					remove the class from the original project's package and attempt a reload"
				projectDef := copiedProjectSetDef 
					projectNamed: theLoadedProject name
					ifAbsent: [ 
						projectDef := theLoadedProject asDefinition.
						copiedProjectSetDef addProject: projectDef.
						projectDef ].
				loadedClass := Rowan image loadedClassNamed: theClassName.
				packageDef := projectDef packageNamed: loadedClass loadedPackage name.
				packageDef removeClassNamed: theClassName.
				processedClassNames add: theClassName ].
	"trim the stack"
	^ self _doProjectSetLoad: copiedProjectSetDef instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames

]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> _loadProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [

	| loadedProjectSet diff loadedProjectInfo| 
	loadedProjectSet := projectSetDefinitionToLoad deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinitionToLoad compareAgainstBase: loadedProjectSet.
	diff isEmpty
		ifTrue: [ 
			| msg |
			msg := 'The projects are already up to date'.
			self inform: msg.
			^ msg ].
	loadedProjectInfo := projectSetDefinitionToLoad properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			"install the packageMapSpecs for this load into the specification prior to the load"
			| projectDefinition |
			projectDefinition := projectSetDefinitionToLoad projectNamed: projectName ifAbsent: [].
			projectDefinition updateGsPlatformSpecLoadedProjectInfo: projectInfo ].
	Rowan image applyModification: diff instanceMigrator: instanceMigrator.
	projectSetDefinitionToLoad definitions
		do: [ :projectDef | 
			self specification: projectDef specification.
			projectDef specification updateLoadedCommitIdForTool: self.
			(loadedProjectInfo at: projectDef name ifAbsent: [])
				ifNotNil: [:map |
					projectDef specification imageSpec
						loadedConfigurationNames: (map at: 'loadedConfigurationNames');
						loadedGroupNames: (map at: 'loadedGroupNames') ]].
	^ diff

]

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
