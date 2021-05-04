Class {
	#name : 'RwPrjLoadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'private' }
RwPrjLoadToolV2 >> _doProjectSetLoad: projectSetDefinition instanceMigrator: instanceMigrator symbolList: symbolList originalProjectSet: originalProjectSet processedClassNames: processedClassNames [
	| copiedProjectSetDef theClassName theClass projectDef theLoadedProject loadedClass packageDef |
	[ 
	^ self
		_loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
		symbolList: symbolList ]
		on: RwExistingVisitorAddingExistingClassNotification
		do: [ :ex | 
			| theProjectName |
			theClassName := ex classDefinition name.
			(processedClassNames includes: theClassName)
				ifTrue: [ ex resume ].
			theClass := ex class.
			theClass isBehavior
				ifFalse: [ ex pass ].
			theProjectName := theClass rowanProjectName.
			theProjectName = Rowan unpackagedName
				ifTrue: [ self error: 'Unexpected unpackaged class ' , theClass name asString printString ]
				ifFalse: [ theLoadedProject := Rowan image loadedProjectNamed: theProjectName ].
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
			loadedClass := Rowan image
				loadedClassForClass: theClass
				ifAbsent: [ self error: 'No loaded class for classs ' , theClassName printString ].
			packageDef := projectDef packageNamed: loadedClass loadedPackage name.
			packageDef removeClassNamed: theClassName.
			processedClassNames add: theClassName ].	"trim the stack"
	^ self
		_doProjectSetLoad: copiedProjectSetDef
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		originalProjectSet: originalProjectSet
		processedClassNames: processedClassNames
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _loadProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator symbolList: symbolList [
	| loadedProjectDefinitionSet diff loadedProjects |
	loadedProjectDefinitionSet := projectSetDefinitionToLoad deriveLoadedProjectSet.	"use loaded things to do the diff - convert to definition when a modification is created"
	projectSetDefinitionToLoad definitions
		keysAndValuesDo: [ :projectName :projectDefinition | 
			projectDefinition packages
				keysAndValuesDo: [ :packageName :packageDefinition | 
					| symdictName |
					"set the target symbol dictionary name for each incoming package definition"
					symdictName := projectDefinition
						gemstoneSymbolDictNameForPackageNamed: packageName.
					packageDefinition gs_symbolDictionary: symdictName.
					packageDefinition classDefinitions values
						do: [ :classDef | classDef gs_symbolDictionary: symdictName ] ] ].
	diff := projectSetDefinitionToLoad
		compareAgainstBaseForLoader: loadedProjectDefinitionSet.
	diff isEmpty
		ifFalse: [ 
			| componentsWithDoits |
			componentsWithDoits := diff componentsWithDoits.
			componentsWithDoits do: [ :component | component executePreloadDoit ].
			Rowan image
				applyModification_V2: diff
				instanceMigrator: instanceMigrator
				symbolList: symbolList.
			componentsWithDoits do: [ :component | component executePostloadDoit ] ].
	loadedProjects := Array new.
	projectSetDefinitionToLoad definitions
		do: [ :projectDef | 
			| theLoadedProject |
			loadedProjects add: (RwProject newNamed: projectDef name).
			theLoadedProject := Rowan image loadedProjectNamed: projectDef name.
			theLoadedProject handle
				_projectComponents: projectDef _projectComponents copy.
			theLoadedProject handle
				_loadSpecification: projectDef _loadSpecification copy.
			theLoadedProject handle
				_projectRepository: projectDef _projectRepository copy.
			theLoadedProject handle
				_projectDefinitionPlatformConditionalAttributes:
					projectDef _projectDefinitionPlatformConditionalAttributes.
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

	^ self
		loadProjectDefinition: projectDefinition
		platformConfigurationAttributes: platformConfigurationAttributes
		instanceMigrator: instanceMigrator
		symbolList: Rowan image symbolList
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes instanceMigrator: instanceMigrator symbolList: symbolList [
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	| projectSetDefinition requiredProjectNames |
	projectSetDefinition := RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself.
	(requiredProjectNames := projectDefinition
		requiredProjectNames: platformConfigurationAttributes) isEmpty
		ifFalse: [ 
			| absentProjectNames |
			"if required projects are not already present in the image, then they must be loaded at this time"
			absentProjectNames := requiredProjectNames
				select: [ :projectName | Rowan projectNamed: projectName ifPresent: [ false ] ifAbsent: [ true ] ].
			absentProjectNames isEmpty
				ifFalse: [ 
					projectSetDefinition := projectDefinition
						readProjectSet: platformConfigurationAttributes ] ].
	^ self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
		symbolList: symbolList
]

{ #category : 'load project by name' }
RwPrjLoadToolV2 >> loadProjectNamed: projectName [
	| projectSet  res |
	projectSet := Rowan projectTools readV2
		readProjectSetForProjectNamed: projectName.
	res := self loadProjectSetDefinition: projectSet.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSet.
	^ res
]

{ #category : 'to be removed' }
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

{ #category : 'to be removed' }
RwPrjLoadToolV2 >> loadProjectNamed: projectName platformConditionalAttributes: platformConditionalAttributes [
       | projectSet res |
       projectSet := Rowan projectTools readV2
               readProjectSetForProjectNamed: projectName
               platformConditionalAttributes: platformConditionalAttributes.
       res := self loadProjectSetDefinition: projectSet.
       "loaded project and loaded packages read from disk - mark them not dirty"
      self markProjectSetNotDirty: projectSet.
       ^ res
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
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
		symbolList: Rowan image symbolList
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinition instanceMigrator: instanceMigrator symbolList: symbolList [
	"NOTE: when loading a definition into a stone, the loaded things are not marked as non-dirty ... dirty state is relative to the disk image for a
		project and a definition can have come from anywhere"

	^ self
		_doProjectSetLoad: projectSetDefinition
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		originalProjectSet: projectSetDefinition
		processedClassNames: Set new
]

{ #category : 'load project definitions' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinitionToLoad symbolList: symbolList [
	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
		symbolList: symbolList
]

{ #category : 'utilities' }
RwPrjLoadToolV2 >> markProjectSetNotDirty: projectSetDefinition [
	"This method should only be used when the projectSetDefinitions have been read from disk - mark them not dirty. Not dirty means that the loaded things match the code on disk."

	projectSetDefinition deriveLoadedThings
		do: [ :loadedProject | 
			loadedProject markNotDirty.
			loadedProject loadedPackages
				valuesDo: [ :loadedPackage | loadedPackage markNotDirty ] ]
]
