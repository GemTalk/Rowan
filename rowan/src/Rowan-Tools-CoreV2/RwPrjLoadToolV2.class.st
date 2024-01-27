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
		on:
			RwExistingVisitorAddingExistingClassNotification
				, RwExistingVisitorAddingExistingMethodNotification
		do: [ :ex | 
			(ex isKindOf: RwExistingVisitorAddingExistingMethodNotification)
				ifTrue: [ 
					| theLoadedMethod theClasDef originalMethodDefinition |
					theLoadedMethod := ex loadedMethod.
					theClassName := ex theClass theNonMetaClass name asString.
					(processedClassNames includes: theClassName)
						ifTrue: [ ex resume ].
					theLoadedProject := ex loadedProject.
					(originalProjectSet projectNamed: theLoadedProject name ifAbsent: [  ])
						ifNotNil: [ :originalProject | 
							| originalPackage originalClassDef |
							originalPackage := originalProject
								packageNamed: theLoadedMethod loadedPackage name.
							originalClassDef := originalPackage
								classDefinitionNamed: theClassName
								ifAbsent: [ 
									originalPackage
										classExtensionDefinitionNamed: theClassName
										ifAbsent: [ ex resume ] ].
							ex theClass isMeta
								ifTrue: [ 
									(originalClassDef classMethodDefinitions includesKey: theLoadedMethod selector)
										ifFalse: [ 
											"method not present in original project, so we can continue with load"
											ex resume ] ]
								ifFalse: [ 
									(originalClassDef instanceMethodDefinitions
										includesKey: theLoadedMethod selector)
										ifFalse: [ 
											"method not present in original project, so we can continue with load"
											ex resume ] ]	"original project needs to be modified, continue with surgery" ].
					copiedProjectSetDef := projectSetDefinition copy.	"a project in the original project set is taking ownership of an already  loaded method,
					remove the method from the original project's package and attempt a reload"
					projectDef := copiedProjectSetDef
						projectNamed: theLoadedProject name
						ifAbsent: [ 
							projectDef := theLoadedProject asDefinition.
							copiedProjectSetDef addProject: projectDef.
							projectDef ].
					packageDef := projectDef packageNamed: theLoadedMethod loadedPackage name.
					theClasDef := packageDef
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ packageDef classDefinitionNamed: theClassName ].
					originalMethodDefinition := ex theClass isMeta
						ifTrue: [ theClasDef removeClassMethod: theLoadedMethod handle selector ]
						ifFalse: [ theClasDef removeInstanceMethod: theLoadedMethod handle selector ].
					originalMethodDefinition source = theLoadedMethod handle sourceString
						ifFalse: [ 
							"method changed in both projects"
							self
								error:
									'Incompatible changes to the same method (' , theClassName
										,
											(ex theClass isMeta
												ifTrue: [ ' class>>' ]
												ifFalse: [ '>>' ]) , originalMethodDefinition selector
										, ' ) in project ' , theLoadedProject name printString
										, ' and project ' , ex incomingProject name printString ] ].
			(ex isKindOf: RwExistingVisitorAddingExistingClassNotification)
				ifTrue: [ 
					| theProjectName |
					theClassName := ex classDefinition name.
					(processedClassNames includes: theClassName)
						ifTrue: [ ex resume ].
					theClass := ex theClass.
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
					processedClassNames add: theClassName ] ].	"trim the stack"
	^ self
		_doProjectSetLoad: copiedProjectSetDef
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		originalProjectSet: originalProjectSet
		processedClassNames: processedClassNames
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _doProjectSetModificationLoad: projectSetModification instanceMigrator: instanceMigrator symbolList: symbolList processedClassNames: processedClassNames [
	"Wrapper method in case we need to handle RwExistingVisitorAddingExistingClassNotification RwExistingVisitorAddingExistingMethodNotification here ... 
		currently this method is used for single method/class updates so we should be able to handle these in the process of create projectSetModification"

	^ [ 
	self
		_loadProjectSetModification: projectSetModification
		instanceMigrator: instanceMigrator
		symbolList: symbolList ]
		on:
			RwExistingVisitorAddingExistingClassNotification
				, RwExistingVisitorAddingExistingMethodNotification
		do: [ :ex | 
			| theClassName theClass theLoadedProject copiedProjectSetDef projectDef loadedClass packageDef |
			(ex isKindOf: RwExistingVisitorAddingExistingClassNotification)
				ifTrue: [ 
					| theProjectName |
					theClassName := ex classDefinition name.
					(processedClassNames includes: theClassName)
						ifTrue: [ ex resume ].
					theClass := ex theClass.
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
					(projectSetModification projectNamed: theLoadedProject name ifAbsent: [  ])
						ifNotNil: [ 
							"If the loadedProject is in the originalProjectSet, then is likely to be a class move - resume and let the chips fall where they may"
							ex resume ].
					copiedProjectSetDef := projectSetModification copy.	"a project in the original project set is taking ownership of an already  loaded class,
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
					processedClassNames add: theClassName ] ]
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _loadProjectDefinition: projectDefinition customConditionalAttributes: customConditionalAttributes instanceMigrator: instanceMigrator symbolList: symbolList [
	"read the components for <projectDefinition> to develop the list of dependent projects"

	| projectSetDefinition requiredProjectNames |
	projectSetDefinition := RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself.
	(requiredProjectNames := projectDefinition
		requiredProjectNames: customConditionalAttributes) isEmpty
		ifFalse: [ 
			| absentProjectNames |
			"if required projects are not already present in the image, then they must be loaded at this time"
			absentProjectNames := requiredProjectNames
				select: [ :projectName | Rowan projectNamed: projectName ifPresent: [ false ] ifAbsent: [ true ] ].
			absentProjectNames isEmpty
				ifFalse: [ 
					projectSetDefinition := projectDefinition
						readProjectSet: customConditionalAttributes ] ].
	^ self
		_doProjectSetLoad: projectSetDefinition
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		originalProjectSet: projectSetDefinition
		processedClassNames: Set new
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _loadProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator symbolList: symbolList [
	| loadedProjectDefinitionSet diff loadedProjects imagePlatformAttributesSet |
	loadedProjectDefinitionSet := projectSetDefinitionToLoad deriveLoadedProjectSet.	"use loaded things to do the diff - convert to definition when a modification is created"
	imagePlatformAttributesSet := Rowan platformConditionalAttributes asSet.
	projectSetDefinitionToLoad definitions
		keysAndValuesDo: [ :projectName :projectDefinition | 
			projectDefinition _projectDefinitionPlatformConditionalAttributes
				ifNil: [ 
					"project definition constructed in memory skip Warning (caveat emptor)"
					 ]
				ifNotNil: [ :projectPlatformAttributes | 
					| projectPlatformAttributesSet |
					"project definition read from disk using platformConditionalAttributes 
						(either default of custom). We expect the attributes to be a reasonable 
						match to the image platform attributes when attempting a load"
					projectPlatformAttributesSet := projectPlatformAttributes asSet.
					projectPlatformAttributesSet remove: 'common' ifAbsent: [  ].	"common doesn't count"
					(imagePlatformAttributesSet * projectPlatformAttributesSet) isEmpty
						ifTrue: [ 
							"If there are no common platform attributes between the current 
								platform and the platformConditionalAttributes used to read the 
								project we'll signal a Warning"
							Warning
								signal:
									'the project ' , projectDefinition projectName printString
										, ' was read from disk with platform conditional attributes ('
										, projectPlatformAttributes printString
										,
											') that is not compatible with the platform conditional attributes for the current image ('
										, Rowan platformConditionalAttributes printString
										, ') so the project may have compile errors when loaded.' ] ].
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
	loadedProjects := RwLoadedProjectSet new.
	projectSetDefinitionToLoad definitions
		do: [ :projectDef | 
			| theLoadedProject |
			loadedProjects addProject: (RwProject newNamed: projectDef name).
			theLoadedProject := Rowan image loadedProjectNamed: projectDef name.
			theLoadedProject handle
				_projectComponents: projectDef _projectComponents copy.
			theLoadedProject handle _loadSpecification: projectDef loadSpecification copy.
			theLoadedProject handle _projectSpecification: projectDef _projectSpecification copy.
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

{ #category : 'private' }
RwPrjLoadToolV2 >> _loadProjectSetModification: projectSetModification instanceMigrator: instanceMigrator symbolList: symbolList [
	| loadedProjects |
	projectSetModification isEmpty
		ifFalse: [ 
			| componentsWithDoits |
			componentsWithDoits := projectSetModification componentsWithDoits.
			componentsWithDoits do: [ :component | component executePreloadDoit ].
			Rowan image
				applyModification_V2: projectSetModification
				instanceMigrator: instanceMigrator
				symbolList: symbolList.
			componentsWithDoits do: [ :component | component executePostloadDoit ] ].
	loadedProjects := RwLoadedProjectSet new.
	projectSetModification elementsModified values
		do: [ :aProjectSetModification | 
			loadedProjects
				addProject: (RwProject newNamed: aProjectSetModification after name) ].
	^ loadedProjects
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

{ #category : 'load project by name' }
RwPrjLoadToolV2 >> loadProjectNamed: projectName instanceMigrator: instanceMigrator [
	| projectSet res |
	projectSet := Rowan projectTools readV2
		readProjectSetForProjectNamed: projectName.
	res := self
		loadProjectSetDefinition: projectSet
		instanceMigrator: instanceMigrator.	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSet.
	^ res
]

{ #category : 'load project set' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinitionToLoad [

	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'load project set' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinition instanceMigrator: instanceMigrator [
	"NOTE: when loading a definition into a stone, the loaded things are not marked as non-dirty ... dirty state is relative to the disk image for a
		project and a definition can have come from anywhere"

	^ self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
		symbolList: Rowan image symbolList
]

{ #category : 'load project set' }
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

{ #category : 'load project set' }
RwPrjLoadToolV2 >> loadProjectSetDefinition: projectSetDefinitionToLoad symbolList: symbolList [
	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
		symbolList: symbolList
]

{ #category : 'load project set modification' }
RwPrjLoadToolV2 >> loadProjectSetModification: projectSetModification instanceMigrator: instanceMigrator symbolList: symbolList [
	"NOTE: when loading a definition into a stone, the loaded things are not marked as non-dirty ... dirty state is relative to the disk image for a
		project and a definition can have come from anywhere"

	^ self
		_doProjectSetModificationLoad: projectSetModification
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		processedClassNames: Set new
]

{ #category : 'load project set modification' }
RwPrjLoadToolV2 >> loadProjectSetModification: projectSetModification symbolList: symbolList [
	^ self
		loadProjectSetModification: projectSetModification
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
