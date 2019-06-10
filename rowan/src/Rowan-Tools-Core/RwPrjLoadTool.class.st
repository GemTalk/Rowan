Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjLoadTool >> _doProjectSetLoad_254: projectSetDefinition instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames [
	| copiedProjectSetDef theClassName theClass projectDef theLoadedProject loadedClass packageDef |
	[ ^ self
		_loadProjectSetDefinition_254: projectSetDefinition
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
	^ self _doProjectSetLoad_254: copiedProjectSetDef instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames

]

{ #category : 'private' }
RwPrjLoadTool >> _loadProjectSetDefinition_254: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [

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
]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadComponentProjectDefinition: projectDefinition [

	^ self loadProjectDefinition: projectDefinition platformConfigurationAttributes: Rowan platformConfigurationAttributes
]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectDefinition: projectDefinition [
	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes [
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	| visitor projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	visitor := Rowan projectTools read
			readConfigurationsForProjectComponentDefinition: projectDefinition
			withConfigurations: projectDefinition loadedConfigurationNames
			groupNames: projectDefinition loadedGroupNames 
			platformConfigurationAttributes: platformConfigurationAttributes
			forLoad: true.
	visitor projectLoadSpecs do: [:loadSpec |
		| dependentProjectDefinition |
		dependentProjectDefinition := loadSpec asDefinition.
		dependentProjectDefinition clone.
		dependentProjectDefinition read do: [:readProjectDef |
			(projectSetDefinition projectNames includes: readProjectDef name)
				ifTrue: [ self error: 'duplicate dependent projects encountered ', readProjectDef name printString]
				ifFalse: [ projectSetDefinition addDefinition: readProjectDef ] ] ].
	^ self loadProjectSetDefinition: projectSetDefinition

]

{ #category : 'load project by spec url' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString [

	| projectDefinition projectSetDefinition res |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : 'load project by spec url' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString  projectRootPath: projectRootPath [

	| projectDefinition projectSetDefinition res |

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : 'load project by spec url' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString withConfigurations: configNames groupNames: groupNames [

	| projectDefinition projectSetDefinition res |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinition := Rowan projectTools read 
		readProjectSetForProjectDefinition: projectDefinition 
		withConfigurations: configNames
		groupNames: groupNames.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]

{ #category : 'load project by spec url' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString  withConfigurations: configNames groupNames: groupNames projectRootPath: projectRootPath [

	| projectDefinition projectSetDefinition res |

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath.
	projectSetDefinition := Rowan projectTools read 
		readProjectSetForProjectDefinition: projectDefinition 
		withConfigurations: configNames
		groupNames: groupNames.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]

{ #category : 'load project by name' }
RwPrjLoadTool >> loadProjectNamed: projectNamed [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'load project by name' }
RwPrjLoadTool >> loadProjectNamed: projectNamed instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed.
	res := self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : 'load project by name' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfigurations: configNames groupNames: groupNames [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed withConfigurations: configNames groupNames: groupNames instanceMigrator: Rowan platform instanceMigrator

]

{ #category : 'load project by name' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfigurations: configNames groupNames: groupNames instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfigurations: configNames 
		groupNames: groupNames.
	res := self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : 'load project by name' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withGroupNames: groupNames [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed withGroupNames: groupNames instanceMigrator: Rowan platform instanceMigrator

]

{ #category : 'load project by name' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withGroupNames: groupNames instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withGroupNames: groupNames.
	res := self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectSetDefinition_254: projectSetDefinitionToLoad [

	^ self
		loadProjectSetDefinition_254: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator

]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectSetDefinition_254: projectSetDefinition instanceMigrator: instanceMigrator [

	"NOTE: when loading a definition into a stone, the loaded things are not marked as non-dirty ... dirty state is relative to the disk image for a
		project and a definition can have come from anywhere"

	^ self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new

]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinitionToLoad [

	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator

]

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinition instanceMigrator: instanceMigrator [

	"NOTE: when loading a definition into a stone, the loaded things are not marked as non-dirty ... dirty state is relative to the disk image for a
		project and a definition can have come from anywhere"

	^ self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new

]

{ #category : 'utilities' }
RwPrjLoadTool >> markProjectSetNotDirty: projectSetDefinition [

	"This method should only be used when the projectSetDefinitions have been read from disk - mark them not dirty. Not dirty means that the loaded things match the code on disk."

	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].

]
