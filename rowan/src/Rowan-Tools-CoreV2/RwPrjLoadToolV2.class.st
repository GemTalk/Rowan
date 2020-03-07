Class {
	#name : 'RwPrjLoadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'private' }
RwPrjLoadToolV2 >> _doProjectSetLoad: projectSetDefinition instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames [
	| copiedProjectSetDef theClassName theClass projectDef theLoadedProject loadedClass packageDef |
	[ 
	^ self
		_loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator ]
		on: RwExistingVisitorAddingExistingClassNotification
		do: [ :ex | 
			theClassName := ex classDefinition name.
			(processedClassNames includes: theClassName)
				ifTrue: [ ex resume ].
			theClass := Rowan globalNamed: theClassName.
			theClass isBehavior
				ifFalse: [ 
					self halt.
					ex pass ].
			theLoadedProject := Rowan image loadedProjectNamed: theClass rowanProjectName.
			theLoadedProject
				ifNil: [ 
					"the loaded project should not be nil - if it is, pass the notification"
					ex pass ].
			(originalProjectSet projectNamed: theLoadedProject name ifAbsent: [  ])
				ifNotNil: [ 
					"If the loadedProject is in the originalProjectSet, then is likely to be a class move - resume and let the chips fall where they may"
					ex resume ].
			copiedProjectSetDef := projectSetDefinition copy.	"a project in the original project set is taking ownership of an already  loaded class,
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
			processedClassNames add: theClassName ].	"trim the stack"
	^ self
		_doProjectSetLoad: copiedProjectSetDef
		instanceMigrator: instanceMigrator
		originalProjectSet: originalProjectSet
		processedClassNames: processedClassNames
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _loadProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [
	| loadedProjectSet loadedProjectDefinitionSet diff loadedProjects |
	loadedProjectSet := projectSetDefinitionToLoad deriveLoadedThings.
	loadedProjectDefinitionSet := loadedProjectSet asProjectDefinitionSet.
	projectSetDefinitionToLoad definitions
		keysAndValuesDo: [ :projectName :projectDefinition | 
			projectDefinition packages
				keysAndValuesDo: [ :packageName :packageDefinition | 
					"set the target symbol dictionary name for each incoming package definition"
					packageDefinition
								gs_symbolDictionary:
									(projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) ] ].
	diff := projectSetDefinitionToLoad
		compareAgainstBaseForLoader: loadedProjectDefinitionSet.
	diff isEmpty
		ifFalse: [ 
			| componentsWithDoits |
			componentsWithDoits := diff componentsWithDoits.
			componentsWithDoits do: [ :component | component executePreloadDoit ].
			Rowan image applyModification_254: diff instanceMigrator: instanceMigrator.
			componentsWithDoits do: [ :component | component executePostloadDoit ] ].
	loadedProjects := Array new.
	projectSetDefinitionToLoad definitions
		do: [ :projectDef | 
			| theLoadedProject |
			loadedProjects add: (RwProject newNamed: projectDef name).
			theLoadedProject := Rowan image loadedProjectNamed: projectDef name.
			theLoadedProject handle _projectStructure: projectDef components copy.
			(projectDef projectDefinitionSourceProperty
				= RwLoadedProject _projectDiskDefinitionSourceValue
				or: [ 
					projectDef projectDefinitionSourceProperty
						= RwLoadedProject _projectLoadedDefinitionSourceWithDependentProjectsValue ])
				ifTrue: [ 
					theLoadedProject
						updateLoadedCommitId;
						markNotDirty.
					theLoadedProject loadedPackages
						valuesDo: [ :loadedPackage | loadedPackage markNotDirty ] ] ].
	^ loadedProjects
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectDefinition: projectDefinition [
	^ self
		loadProjectDefinition: projectDefinition
		platformConfigurationAttributes: projectDefinition platformConditionalAttributes
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectDefinition: projectDefinition instanceMigrator: instanceMigrator [
	^ self
		loadProjectDefinition: projectDefinition
		platformConfigurationAttributes: projectDefinition platformConditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes [
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	^ self
		loadProjectDefinition: projectDefinition
		platformConfigurationAttributes: platformConfigurationAttributes
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes instanceMigrator: instanceMigrator [
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new
				addProject: projectDefinition;
				yourself.
	^ self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
]

{ #category : 'load project by name' }
RwPrjLoadToolV2 >> loadProjectNamed: projectName [
	| projectSet |
	projectSet := Rowan projectTools readV2
		readProjectSetForProjectNamed: projectName.
	^ self loadProjectSetDefinition: projectSet
]

{ #category : 'load project by name' }
RwPrjLoadToolV2 >> loadProjectNamed: projectName customConditionalAttributes: customConditionalAttributes [
	| platformConditionalAttributes project |
	project := RwProject newNamed: projectName.
	platformConditionalAttributes := project platformConditionalAttributes copy
		asSet.
	platformConditionalAttributes addAll: customConditionalAttributes.
	^ self
		loadProjectNamed: projectName
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'load project by name' }
RwPrjLoadToolV2 >> loadProjectNamed: projectName platformConditionalAttributes: platformConditionalAttributes [
	| projectSet |
	projectSet := Rowan projectTools readV2
		readProjectSetForProjectNamed: projectName
		platformConditionalAttributes: platformConditionalAttributes.
	^ self loadProjectSetDefinition: projectSet
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinitionToLoad [

	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinition instanceMigrator: instanceMigrator [

	"NOTE: when loading a definition into a stone, the loaded things are not marked as non-dirty ... dirty state is relative to the disk image for a
		project and a definition can have come from anywhere"

	^ self 
		_doProjectSetLoad: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new
]
