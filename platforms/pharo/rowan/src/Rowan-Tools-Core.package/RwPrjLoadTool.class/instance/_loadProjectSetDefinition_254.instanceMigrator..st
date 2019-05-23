private
_loadProjectSetDefinition_254: projectSetDefinitionToLoad instanceMigrator: instanceMigrator

	| loadedProjectSet loadedProjectDefinitionSet diff loadedProjectInfo| 
	loadedProjectSet := projectSetDefinitionToLoad deriveLoadedThings.
	loadedProjectDefinitionSet := loadedProjectSet asProjectDefinitionSet.
	loadedProjectInfo := projectSetDefinitionToLoad properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			"install the packageMapSpecs for this load into the specification prior to the load"
			| projectDefinition |
			projectDefinition := projectSetDefinitionToLoad projectNamed: projectName ifAbsent: [].
			projectDefinition updateGsPlatformSpecLoadedProjectInfo: projectInfo ].
	projectSetDefinitionToLoad definitions keysAndValuesDo: [:projectName :projectDefinition |
			projectDefinition packages keysAndValuesDo: [:packageName :packageDefinition |
				"set the target symbol dictionary name for each incoming package definition"
				packageDefinition gs_symbolDictionary: (projectDefinition symbolDictNameForPackageNamed: packageName) ] ].
	diff := projectSetDefinitionToLoad compareAgainstBase_254: loadedProjectDefinitionSet.
	diff isEmpty
		ifFalse: [  Rowan image applyModification_254: diff instanceMigrator: instanceMigrator ].
	projectSetDefinitionToLoad definitions
		do: [ :projectDef |
			| theProjectDef |
			theProjectDef := (loadedProjectSet entities at: projectDef name ifAbsent: [])
				ifNil: [ projectDef ]
				ifNotNil: [:loadedProject | loadedProject ].
			projectDef projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue
				ifTrue: [  theProjectDef updateLoadedCommitId ].
			(loadedProjectInfo at: projectDef name ifAbsent: [])
				ifNotNil: [:map |
					theProjectDef
						loadedConfigurationNames: (map at: 'loadedConfigurationNames');
						loadedGroupNames: (map at: 'loadedGroupNames') ] ].
	^ diff