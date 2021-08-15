Class {
	#name : 'RwPrjUpgradeTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjUpgradeTool >> _doProjectSetUpgrade: projectSetDefinition instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames [
	| copiedProjectSetDef theClassName theClass projectDef theLoadedProject loadedClass packageDef |
	[ ^ self
		_upgradeProjectSetDefinition: projectSetDefinition
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
	^ self _doProjectSetUpgrade: copiedProjectSetDef instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames
]

{ #category : 'private' }
RwPrjUpgradeTool >> _upgradeProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [

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
	diff := projectSetDefinitionToLoad compareAgainstBase_forUpgrade: loadedProjectDefinitionSet.
	diff isEmpty
		ifFalse: [  Rowan image applyModification_254: diff instanceMigrator: instanceMigrator ].
	projectSetDefinitionToLoad definitions
		do: [ :projectDef |
			| theSpec |
			theSpec := (loadedProjectSet entities at: projectDef name ifAbsent: [])
				ifNil: [ projectDef specification ]
				ifNotNil: [:loadedProject | loadedProject specification ].
			self specification: theSpec.
			projectDef projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue
				ifTrue: [  theSpec updateLoadedCommitIdForTool: self ].
			(loadedProjectInfo at: projectDef name ifAbsent: [])
				ifNotNil: [:map |
					theSpec imageSpec
						loadedConfigurationNames: (map at: 'loadedConfigurationNames');
						loadedGroupNames: (map at: 'loadedGroupNames') ] ].
	^ diff
]

{ #category : 'utilities' }
RwPrjUpgradeTool >> markProjectSetNotDirty: projectSetDefinition [

	"This method should only be used when the projectSetDefinitions have been read from disk - mark them not dirty. Not dirty means that the loaded things match the code on disk."

	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].

]

{ #category : 'upgradeproject by spec url' }
RwPrjUpgradeTool >> upgradeProjectFromSpecUrl: fileUrlOrString [

	| projectDefinition projectSetDefinition res |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	res := self
		_doProjectSetUpgrade: projectSetDefinition
		instanceMigrator: Rowan platform instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]
