Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
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

{ #category : 'smalltalk api' }
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
			| projectDefinition spec gemstoneSpec thePackageMapSpecs |
			projectDefinition := projectSetDefinitionToLoad projectNamed: projectName ifAbsent: [].
			spec := projectDefinition specification.
			thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
			gemstoneSpec := spec platformSpec at: 'gemstone'.
			(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
				ifNotNil: [:name | gemstoneSpec defaultSymbolDictName: name ].
			(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
				ifNotNil: [:boolean | 
					gemstoneSpec defaultUseSessionMethodsForExtensions: boolean  ].
			(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
				ifNotNil: [:map | gemstoneSpec packageNameToPlatformPropertiesMap: map] ].
	Rowan image applyModification: diff instanceMigrator: instanceMigrator.
	projectSetDefinitionToLoad definitions
		do: [ :project | 
			self specification: project specification.
			project specification updateLoadedCommitIdForTool: self.
			(loadedProjectInfo at: project name ifAbsent: [])
				ifNotNil: [:map |
					project specification imageSpec
						loadedConfigurationNames: (map at: 'loadedConfigurationNames');
						loadedGroupNames: (map at: 'loadedGroupNames') ]].
	^ diff

]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectDefinition: projectDefinition [

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectNamed: projectNamed [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'smalltalk api' }
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

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinitionToLoad [

	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinition instanceMigrator: instanceMigrator [

	^ self
		_doProjectSetLoad: projectSetDefinition 
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new

]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadSpecUrl: aString [
  ^ self loadSpecification: (RwSpecification fromUrl: aString)
]
