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

{ #category : 'private' }
RwPrjLoadTool >> _loadProjectSetDefinition_254: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [

	| loadedProjectSet diff loadedProjectInfo| 
	loadedProjectSet := projectSetDefinitionToLoad deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinitionToLoad compareAgainstBase_254: loadedProjectSet.
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
	Rowan image applyModification_254: diff instanceMigrator: instanceMigrator.
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

{ #category : 'private' }
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

{ #category : 'load project definitions' }
RwPrjLoadTool >> loadProjectDefinition: projectDefinition [

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'load project by spec url' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString [

	| projectDefinition projectSetDefinitionToLoad |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinitionToLoad := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	^ self loadProjectSetDefinition: projectSetDefinitionToLoad

]

{ #category : 'load project by spec url' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString  projectRootPath: projectRootPath [

	| projectDefinition projectSetDefinitionToLoad |

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath.
	projectSetDefinitionToLoad := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	^ self loadProjectSetDefinition: projectSetDefinitionToLoad

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
		_doProjectSetLoad: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].
	^ res

]

{ #category : 'deprecation candidates' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
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

{ #category : 'deprecation candidates' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
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
		_doProjectSetLoad: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new

]
