smalltalk api
reconcileGlobalClassExtensionsForProjectFromSpecUrl: specUrl defaultGroupName: defaultGroupName globalsGroupName: globalsGroupName  globalsUserId: globalsUserId writeProject: writeProjectBool
	| projectDefinition projectDefinitionSet projectTools loadedProjectInfo configExportMap |
	projectTools := Rowan projectTools.
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrl.
	projectDefinitionSet := projectTools read readProjectSetForProjectDefinition: projectDefinition.

	loadedProjectInfo := projectDefinitionSet properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			"install the packageMapSpecs for this load into the specification prior to the reconcile"
			(projectDefinitionSet projectNamed: projectName ifAbsent: [])
				updateGsPlatformSpecLoadedProjectInfo: projectInfo ].

	configExportMap := self 
		reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet 
		defaultGroupName: defaultGroupName 
		globalsGroupName: globalsGroupName  
		globalsUserId: globalsUserId.
	writeProjectBool
		ifTrue: [
			configExportMap keysAndValuesDo: [:config :exportUrl |
				config exportToUrl: exportUrl ].
			projectTools write writeProjectDefinition: projectDefinition ].
	^ projectDefinition
