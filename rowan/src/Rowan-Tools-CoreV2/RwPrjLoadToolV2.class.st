Class {
	#name : 'RwPrjLoadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'private' }
RwPrjLoadToolV2 >> _doProjectSetLoad: projectSetDefinition instanceMigrator: instanceMigrator symbolList: symbolList originalProjectSet: originalProjectSetDefinition processedClassNames: processedClassNames [
	| workingProjectSetDef theOriginalWorkingProjectSetDefinition loadedProjectSetDef |
	Rowan projectTools trace trace: '[RwPrjLoadToolV2] [BEGIN _doProjectSetLoad]'.
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
					| theClassName incomingPackage incomingProject theLoadedProject theLoadedMethod |
					Rowan projectTools trace
						trace: '[RwPrjLoadToolV2] [begin method notification]'.
					theClassName := ex theClass theNonMetaClass name asString.
					incomingPackage := ex incomingPackage.
					incomingProject := ex incomingProject.
					(processedClassNames includes: theClassName)
						ifTrue: [ 
							"we've processed this class, but have we processed extension methods?"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] THE CLASS ' , theClassName , ' in the package named '
										, incomingPackage name
										, ' is in PROCESSED CLASSES for existing method ... RESUMING'.
							ex resume ].
					theLoadedProject := ex loadedProject.
					theLoadedMethod := ex loadedMethod.
					(RwExistingVisitorChangingPackageOwnershipNotification new
						loadedClassOrMethodDefinition: theLoadedMethod;
						incomingProject: incomingProject;
						incomingPackage: incomingPackage;
						yourself) signal
						ifTrue: [ 
							"incoming wins"
							(originalProjectSetDefinition
								projectNamed: theLoadedProject name
								ifAbsent: [  ])
								ifNotNil: [ :originalProject | 
									| originalPackage originalClassDef |
									originalPackage := originalProject
										packageNamed: theLoadedMethod loadedPackage name.
									originalClassDef := originalPackage
										classDefinitionNamed: theClassName
										ifAbsent: [ 
											originalPackage
												classExtensionDefinitionNamed: theClassName
												ifAbsent: [ 
													"class not modified in original project, so we can continue with load"
													ex resume ] ].
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
													ex resume ] ] ].
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Moving method definition ' , theClassName
										,
											(ex theClass isMeta
												ifTrue: [ ' class' ]
												ifFalse: [ '' ]) , '>>' , theLoadedMethod handle selector
										, ' into the package ' , incomingPackage name.	"
							original project set contains a modification to a loaded method that is in a different project"
							(self
								_scanProjectSet: projectSetDefinition
								exMethodNotification: ex
								originalProjectSet: originalProjectSetDefinition)
								ifNotNil: [ :return | workingProjectSetDef := return ]
								ifNil: [ 
									"resume exception"
									ex resume ].
							Rowan projectTools trace trace: '[RwPrjLoadToolV2] [end incoming wins]' ]
						ifFalse: [ 
							"incoming loses, preserve loaded project ownership"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Preserving method definition ' , theClassName
										,
											(ex theClass isMeta
												ifTrue: [ ' class' ]
												ifFalse: [ '' ]) , '>>' , theLoadedMethod handle selector
										, ' in the package ' , theLoadedMethod loadedPackage name.
							(self
								_updateProjectSetForOriginalProjectModification: projectSetDefinition
								exMethodNotification: ex
								className: theClassName
								incomingPackageName: incomingPackage name
								incomingProjectName: incomingProject name
								loadedMethod: theLoadedMethod
								loadedProject: theLoadedProject
								originalProjectSet: originalProjectSetDefinition)
								ifNotNil: [ :return | workingProjectSetDef := return ]
								ifNil: [ 
									"resume exception"
									ex resume ].
							((workingProjectSetDef _attributes
								at: 'modifiedProjects'
								ifAbsentPut: [ Dictionary new ])
								at: incomingProject name
								ifAbsentPut: [ Set new ]) add: incomingPackage name.
							Rowan projectTools trace trace: '[RwPrjLoadToolV2] [end incoming loses]' ].
					Rowan projectTools trace
						trace: '[RwPrjLoadToolV2] [end method notification]' ].
			(ex isKindOf: RwExistingVisitorAddingExistingClassNotification)
				ifTrue: [ 
					| theClassName incomingPackage theLoadedProject |
					Rowan projectTools trace
						trace: '[RwPrjLoadToolV2] [begin class notification]'.
					theClassName := ex classDefinition name.
					theLoadedProject := ex loadedProject.
					incomingPackage := ex incomingPackage.
					(processedClassNames includes: theClassName)
						ifTrue: [ 
							"we've processed this class, but have we processed extension methods?"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] THE CLASS ' , theClassName , ' in the package named '
										, incomingPackage name
										, ' is in PROCESSED CLASSES for existing class... RESUMING'.
							ex resume ].
					(RwExistingVisitorChangingPackageOwnershipNotification new
						loadedClassOrMethodDefinition: ex loadedClass) signal
						ifTrue: [ 
							Rowan projectTools trace trace: '[RwPrjLoadToolV2] [start class incoming wins]'.	"incoming wins"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Moving class definition ' , theClassName
										, ' into the package ' , incomingPackage name.
							(self
								_scanProjectSet: projectSetDefinition
								exClassNotification: ex
								originalProjectSet: originalProjectSetDefinition)
								ifNotNil: [ :return | workingProjectSetDef := return ]
								ifNil: [ 
									"resume exception"
									ex resume ].
							Rowan projectTools trace
								trace: '[RwPrjLoadToolV2] [end class incoming wins]' ]
						ifFalse: [ 
							"incoming loses, preserve loaded project ownership"
							Rowan projectTools trace
								trace: '[RwPrjLoadToolV2] [start class incoming losess]'.
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Preserving class definition ' , theClassName
										, ' in the package ' , ex loadedClass loadedPackage name.
							(self
								_updateProjectSetForOriginalProjectModification: projectSetDefinition
								exClassNotification: ex
								className: theClassName
								loadedProject: theLoadedProject
								originalProjectSet: originalProjectSetDefinition
								processedClassNames: processedClassNames)
								ifNotNil: [ :return | workingProjectSetDef := return ]
								ifNil: [ 
									"resume exception"
									ex resume ].
							Rowan projectTools trace
								trace: '[RwPrjLoadToolV2] [end start class incoming losess]' ].
					Rowan projectTools trace
						trace: '[RwPrjLoadToolV2] [end class notification]' ].
			Rowan projectTools trace trace: '[RwPrjLoadToolV2] [about to trim stack]' ].	"
	trim the stack ... We've made modifications to the originalProjectSetDefinition in the
		workingProjectSetDef, so we need rerun the load using workingProjectSetDef"
	Rowan projectTools trace
		trace: '[RwPrjLoadToolV2] [trim the stack and recurse]'.
	theOriginalWorkingProjectSetDefinition := workingProjectSetDef
		ifNil: [ originalProjectSetDefinition ]
		ifNotNil: [ 
			"treat the workingProjectSetDef as originalProjectSetDefinition because it contains edits to the originalProjectSetDefinition that must be preserved"
			workingProjectSetDef ].
	loadedProjectSetDef := self
		_doProjectSetLoad: workingProjectSetDef
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		originalProjectSet: theOriginalWorkingProjectSetDefinition
		processedClassNames: processedClassNames.
	(workingProjectSetDef _attributes
		at: 'modifiedProjects'
		ifAbsent: [ Dictionary new ])
		keysAndValuesDo: [ :modifiedProjectName :modifiedPackageNames | 
			| modLoadedPackages |
			"mark the packages that 'lost' as dirty, since they no longer match
				the original project definition (incoming loses and loaded loses"
			modLoadedPackages := (loadedProjectSetDef projectNamed: modifiedProjectName)
				_concreteProject loadedPackages.
			modifiedPackageNames
				do: [ :modPackageName | (modLoadedPackages at: modPackageName) markDirty ] ].
	^ loadedProjectSetDef
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
RwPrjLoadToolV2 >> _scanFullProjectSetForMoves: workingProjectSetDef notification: ex loadedMethod: theLoadedMethodOrNil loadedClass: theLoadedClassOrNil originalProjectSet: originalProjectSetDefinition [
	"incoming wins.
	... return nil to resume exception ... need to recurse the _doProjectSet... call"

	originalProjectSetDefinition
		namesAndDefinitions: [ :originalProjectName :originalProjectDefinition | 
			originalProjectDefinition packages
				keysAndValuesDo: [ :originalPackageName :originalPackageDefinition | 
					originalPackageDefinition classExtensions
						keysAndValuesDo: [ :originalClassName :originalClassDefinition | 
							originalClassDefinition instanceMethodDefinitions
								keysAndValuesDo: [ :originalSelector :originalMethodDefinition | 
									self
										_updateProjectSetForScan: theLoadedMethodOrNil
										notification: ex
										projectDefinition: originalProjectDefinition
										packageDefinition: originalPackageDefinition
										className: originalClassName
										isMeta: false
										selector: originalSelector
										methodDefinition: originalMethodDefinition
										projectSet: workingProjectSetDef
										originalProjectSet: originalProjectSetDefinition ].
							originalClassDefinition classMethodDefinitions
								keysAndValuesDo: [ :originalSelector :originalMethodDefinition | 
									self
										_updateProjectSetForScan: theLoadedMethodOrNil
										notification: ex
										projectDefinition: originalProjectDefinition
										packageDefinition: originalPackageDefinition
										className: originalClassName
										isMeta: true
										selector: originalSelector
										methodDefinition: originalMethodDefinition
										projectSet: workingProjectSetDef
										originalProjectSet: originalProjectSetDefinition ] ].
					originalPackageDefinition classDefinitions
						keysAndValuesDo: [ :originalClassName :originalClassDefinition | 
							self
								_updateProjectSetForScan: theLoadedClassOrNil
								notification: ex
								projectDefinition: originalProjectDefinition
								packageDefinition: originalPackageDefinition
								className: originalClassName
								projectSetDefinition: workingProjectSetDef
								originalProjectSet: originalProjectSetDefinition ] ] ]
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _scanProjectSet: projectSetDefinition exClassNotification: existingClassNotification originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method that is in a different project ... and we are in incoming wins mode.
	... return nil to resume exception ... need to recurse the _doProjectSet... call"

	| theLoadedProject theLoadedPackage theClass incomingProject incomingPackage workingProjectSetDef theLoadedClass theClassName |
	theClassName := existingClassNotification theClass theNonMetaClass name
		asString.
	incomingProject := existingClassNotification incomingProject.
	incomingPackage := existingClassNotification incomingPackage.
	theClass := existingClassNotification theClass.
	theLoadedClass := existingClassNotification loadedClass.
	theLoadedProject := existingClassNotification loadedProject.
	theLoadedPackage := existingClassNotification loadedPackage.	"
	copy (if needed) and update the projectSetDefinition, moving the method from 
		theLoadedPackage to the incomingPackage"
	(self
		_updateProjectSet: projectSetDefinition
		incomingProject: incomingProject
		incomingPackage: incomingPackage
		notification: existingClassNotification
		theClassName: theClassName
		theLoadedClass: theLoadedClass
		theLoadedProject: theLoadedProject
		theLoadedPackage: theLoadedPackage
		originalProjectSet: originalProjectSetDefinition)
		ifNotNil: [ :return | workingProjectSetDef := return ]
		ifNil: [ ^ nil	"resume" ].	"
	while we're in the neighborhood, scan the originalProjectSetDefinition for any loaded 
		method definitions that are not in a different project than the incoming project
		for the method. It's cheaper to do this in one pass, than to use the loader
		to find them. We'll let the loader find class definitions that are in a different project,
		as this happens less frequently."
	(self
		_scanFullProjectSetForMoves: workingProjectSetDef
		notification: existingClassNotification
		loadedMethod: nil
		loadedClass: theLoadedClass
		originalProjectSet: originalProjectSetDefinition) ifNil: [ ^ nil	"resume" ].
	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _scanProjectSet: projectSetDefinition exMethodNotification: existingMethodNotification originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method that is in a different project ... and we are in incoming wins mode.
	... return nil to resume exception ... need to recurse the _doProjectSet... call"

	| theLoadedMethod theLoadedProject theLoadedPackage theClass incomingProject incomingPackage theClassName workingProjectSetDef theLoadedClass |
	theClassName := existingMethodNotification theClass theNonMetaClass name
		asString.
	incomingProject := existingMethodNotification incomingProject.
	incomingPackage := existingMethodNotification incomingPackage.
	theClass := existingMethodNotification theClass.
	theLoadedMethod := existingMethodNotification loadedMethod.
	theLoadedClass := theLoadedMethod loadedClass.
	theLoadedProject := existingMethodNotification loadedProject.
	theLoadedPackage := existingMethodNotification loadedPackage.	"
	copy (if needed) and update the projectSetDefinition, moving the method from 
		theLoadedPackage to the incomingPackage"
	(self
		_updateProjectSet: projectSetDefinition
		incomingProject: incomingProject
		incomingPackage: incomingPackage
		notification: existingMethodNotification
		theClass: theClass
		theLoadedMethod: theLoadedMethod
		theLoadedProject: theLoadedProject
		theLoadedPackage: theLoadedPackage
		originalProjectSet: originalProjectSetDefinition)
		ifNotNil: [ :return | workingProjectSetDef := return ]
		ifNil: [ ^ nil	"resume" ].	"
	while we're in the neighborhood, scan the originalProjectSetDefinition for any loaded 
		method definitions that are not in a different project than the incoming project
		for the method. It's cheaper to do this in one pass, than to use the loader
		to find them. We'll let the loader find class definitions that are in a different project,
		as this happens less frequently."
	(self
		_scanFullProjectSetForMoves: workingProjectSetDef
		notification: existingMethodNotification
		loadedMethod: theLoadedMethod
		loadedClass: nil
		originalProjectSet: originalProjectSetDefinition) ifNil: [ ^ nil	"resume" ].
	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSet: projectSetDefinition incomingProject: incomingProject incomingPackage: incomingPackage notification: ex theClass: theClass theLoadedMethod: theLoadedMethod theLoadedProject: theLoadedProject theLoadedPackage: theLoadedPackage originalProjectSet: originalProjectSetDefinition [
	"incoming wins ... original project set contains a modification to a method that is in a different project.
		MOVE the definition from the loaded project/package and add it to the incoming project/package
	... return nil to resume exception ... need to recurse the _doProjectSet... call"

	| theClasDef originalMethodDefinition theClassName workingPackageDef workingProjectDef workingProjectSetDef |
	theClassName := theClass theNonMetaClass name asString.	"
	confirm that the definition for theLoadedMethod is in the original project"
	(originalProjectSetDefinition
		projectNamed: incomingProject name
		ifAbsent: [ 
			"handled later"
			 ])
		ifNotNil: [ :originalProject | 
			| originalPackage originalClassDef |
			originalPackage := originalProject
				packageNamed: incomingPackage name
				ifAbsent: [ 
					"incoming package not in original project set ... no ownership conflict ... continue with load"
					^ nil "resume" ].
			originalClassDef := originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					originalPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ 
							"incoming class definition not in original project set ... no ownership conflict ... continue with load"
							^ nil "resume" ] ].
			theClass isMeta
				ifTrue: [ 
					(originalClassDef classMethodDefinitions includesKey: theLoadedMethod selector)
						ifFalse: [ 
							"incoming class method not in original project set ... no ownership conflict ... continue with load"
							^ nil "resume" ] ]
				ifFalse: [ 
					(originalClassDef instanceMethodDefinitions
						includesKey: theLoadedMethod selector)
						ifFalse: [ 
							"incoming instance method not in original project set ... no ownership conflict ... continue with load"
							^ nil "resume" ] ] ].
	Rowan projectTools trace
		trace:
			'[RwPrjLoadToolV2] Loaded project ' , theLoadedProject name
				, ' manages method definition of ' , theClassName
				,
					(theClass isMeta
						ifTrue: [ ' class' ]
						ifFalse: [ '' ]) , '>>' , theLoadedMethod selector , ', incoming project '
				, incomingProject name , ' and package ' , incomingPackage name
				, ' modifies the method.'.	"
	confirm that the definition for theLoadedMethod is in the loadedProject project"
	workingProjectSetDef := originalProjectSetDefinition == projectSetDefinition
		ifTrue: [ projectSetDefinition copy ]
		ifFalse: [ 
			"copy was made in an earlier iteration"
			projectSetDefinition ].	"
	a project in the incoming project set is taking ownership of an already  loaded method,
	remove the method from the loaded project's package and attempt a reload ... incoming wins"
	workingProjectDef := workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			| projectDef |
			"adding copy of the theloadedProject to the copied project"
			projectDef := theLoadedProject asDefinition.
			workingProjectSetDef addProject: projectDef.
			projectDef ].	"
	confirm that the definition for theLoadedMethod is in the working project ... 
		we will be removing the definition from the loaded project ... incoming wins"
	(workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			"not expected .... we added it just above"
			self
				error:
					'no project named ' , theLoadedProject name
						, ' found in working projectSet project set.' ])
		ifNotNil: [ :theWorkingLoadedProject | 
			| theWorkingLoadedPackage theWorkingLoadedClassDef |
			theWorkingLoadedPackage := theWorkingLoadedProject
				packageNamed: theLoadedPackage name.
			theWorkingLoadedClassDef := theWorkingLoadedPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					theWorkingLoadedPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ 
							"incoming class definition not in working project set ... no ownership conflict ... continue with load"
							^ nil "resume" ] ].
			theClass isMeta
				ifTrue: [ 
					(theWorkingLoadedClassDef classMethodDefinitions
						includesKey: theLoadedMethod selector)
						ifFalse: [ 
							"incoming class method definition not in working project set ... no ownership conflict ... continue with load"
							^ nil "resume" ] ]
				ifFalse: [ 
					(theWorkingLoadedClassDef instanceMethodDefinitions
						includesKey: theLoadedMethod selector)
						ifFalse: [ 
							"incoming instance method definition not in working project set ... no ownership conflict ... continue with load"
							^ nil "resume" ] ] ].	"
	remove the definition from the working loaded project"
	workingPackageDef := workingProjectDef packageNamed: theLoadedPackage name.
	theClasDef := workingPackageDef
		classExtensionDefinitionNamed: theClassName
		ifAbsent: [ workingPackageDef classDefinitionNamed: theClassName ].	"
	Remove the method definition from the working loaded project ... it is already 
		present in the the working loaded package, so no need for explicit add. "
	originalMethodDefinition := theClass isMeta
		ifTrue: [ theClasDef removeClassMethod: theLoadedMethod handle selector ]
		ifFalse: [ theClasDef removeInstanceMethod: theLoadedMethod handle selector ].
	Rowan projectTools trace
		trace:
			'[RwPrjLoadToolV2] Moving method definition ' , theClassName , '>>'
				, theLoadedMethod handle selector , ' from package '
				, workingPackageDef name , ' into the package ' , incomingPackage name.
	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSet: projectSetDefinition incomingProject: incomingProject incomingPackage: incomingPackage notification: ex theClassName: theClassName theLoadedClass: theLoadedClass theLoadedProject: theLoadedProject theLoadedPackage: theLoadedPackage originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method or class that is in a different project.
		MOVE the definition from the loaded project/package and add it to the incoming project/package
	 ... return nil to resume exception ... need to recurse the _doProjectSet... call"

	| workingProjectSetDef workingProjectDef workingPackageDef originalClassDef |
	"confirm that the definition for theLoadedClass is in the original project"
	(originalProjectSetDefinition
		projectNamed: incomingProject name
		ifAbsent: [ 
			"handled later"
			 ])
		ifNotNil: [ :originalProject | 
			| originalPackage |
			originalPackage := originalProject
				packageNamed: incomingPackage name
				ifAbsent: [ 
					"incoming package not in original project set ... no ownership conflict ... continue with load"
					^nil "resume"  ].
			originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					originalPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ 
							"incoming class definition not in original project set ... no ownership conflict ... continue with load"
							^nil "resume"  ] ] ].
	Rowan projectTools trace
		trace:
			'[RwPrjLoadToolV2] Loaded project ' , theLoadedProject name
				, ' manages the class definition of ' , theClassName , ', incoming project '
				, incomingProject name , ' and package ' , incomingPackage name
				, ' modifies the class.'.	"
	confirm that the definition for theLoadedMethod is in the loadedProject project"
	workingProjectSetDef := originalProjectSetDefinition == projectSetDefinition
		ifTrue: [ projectSetDefinition copy ]
		ifFalse: [ 
			"copy was made in an earlier iteration"
			projectSetDefinition ].	"
	a project in the incoming project set is taking ownership of an already  loaded class,
	remove the class from the loaded project's package and attempt a reload"
	workingProjectDef := workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			| projectDef |
			"adding copy of the theloadedProject to the copied project"
			projectDef := theLoadedProject asDefinition.
			workingProjectSetDef addProject: projectDef.
			projectDef ].	"
	confirm that the definition for theLoadedClass is in the working project"
	(workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			self
				error:
					'no project named ' , theLoadedProject name
						, ' found in working projectSet project set.' ])
		ifNotNil: [ :originalProject | 
			| originalPackage |
			originalPackage := originalProject packageNamed: theLoadedPackage name.
			originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					self
						error:
							'No class definition for the class ' , theClassName
								, ' in the working package named ' , theLoadedProject name
								, ' during load processing.' ] ].
	workingPackageDef := workingProjectDef packageNamed: theLoadedPackage name.	"
	Remove the class definition from the working project ... it is already 
		present in the incoming package, so no need for explicit add. "
	originalClassDef := workingPackageDef removeClassNamed: theClassName.
	Rowan projectTools trace
		trace:
			'[RwPrjLoadToolV2] Moving class definition ' , theClassName , ' from package '
				, workingPackageDef name , ' into the package ' , incomingPackage name.
	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForOriginalProjectModification: projectSetDefinition exClassNotification: existingClassNotification className: theClassName loadedProject: theLoadedProject originalProjectSet: originalProjectSetDefinition processedClassNames: processedClassNames [
	"the loaded project wins, so modify and return a copy of the (incoming) projectSetDefinition with the conflicting class definition removed..
	... return nil to resume exception ... need to recurse the _doProjectSet... call"

	| workingProjectSetDef projectDef theClass loadedClass packageDef |
	theClass := existingClassNotification theClass.
	workingProjectSetDef := originalProjectSetDefinition == projectSetDefinition
		ifTrue: [ projectSetDefinition copy ]
		ifFalse: [ 
			"copy was made in an earlier iteration"
			projectSetDefinition ].
	projectDef := workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			projectDef := theLoadedProject asDefinition.
			workingProjectSetDef addProject: projectDef.
			projectDef ].
	loadedClass := Rowan image
		loadedClassForClass: theClass
		ifAbsent: [ self error: 'No loaded class for classs ' , theClassName printString ].
	packageDef := projectDef packageNamed: loadedClass loadedPackage name.
	packageDef removeClassNamed: theClassName.
	processedClassNames add: theClassName.
	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForOriginalProjectModification: projectSetDefinition exMethodNotification: ex className: theClassName incomingPackageName: incomingPackageName  incomingProjectName: incomingProjectName loadedMethod: theLoadedMethod loadedProject: theLoadedProject originalProjectSet: originalProjectSetDefinition [
	"loaded project wins ... modify and return a copy of the incoming projectSetDefinition with the conflicting method definition removed 
		... return nil to resume exception ... need to recurse the _doProjectSet... call"

	| theClasDef copiedProjectSetDef projectDef packageDef |
	(originalProjectSetDefinition projectNamed: incomingProjectName ifAbsent: [  ])
		ifNotNil: [ :originalProject | 
			| originalPackage originalClassDef |
			originalPackage := originalProject packageNamed: incomingPackageName.
			originalClassDef := originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					originalPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ ^nil "resume" ] ].
			ex theClass isMeta
				ifTrue: [ 
					(originalClassDef classMethodDefinitions includesKey: theLoadedMethod selector)
						ifFalse: [ 
							"method not present in original project, so we can continue with load"
							^ nil "resume" ] ]
				ifFalse: [ 
					(originalClassDef instanceMethodDefinitions
						includesKey: theLoadedMethod selector)
						ifFalse: [ 
							"method not present in original project, so we can continue with load"
							^ nil "resume" ] ]	"original project needs to be modified, continue with surgery" ].
	copiedProjectSetDef := originalProjectSetDefinition == projectSetDefinition
		ifTrue: [ projectSetDefinition copy ]
		ifFalse: [ 
			"copy was made in an earlier iteration"
			projectSetDefinition ].	"
	a loaded project is preserving ownership of an already  loaded method,
		remove the method from the incoming project's package and attempt a reload"
	projectDef := copiedProjectSetDef projectNamed: incomingProjectName.
	packageDef := projectDef packageNamed: incomingPackageName.
	theClasDef := packageDef
		classExtensionDefinitionNamed: theClassName
		ifAbsent: [ packageDef classDefinitionNamed: theClassName ].
	ex theClass isMeta
		ifTrue: [ theClasDef removeClassMethod: theLoadedMethod handle selector ]
		ifFalse: [ theClasDef removeInstanceMethod: theLoadedMethod handle selector ].
	^ copiedProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForScan: oLoadedMethodOrNil notification: ex projectDefinition: oProjectDefinition packageDefinition: oPackageDefinition className: className isMeta: isMeta selector: oSelector methodDefinition: oMethodDefinition projectSet: projectSetDefinition originalProjectSet: originalProjectSetDefinition [
	"oLoadedMethod is the original loaded method that kicked off this scan ... and the prupose of this scan is to 
	scan the REST of the changes, excluding oLoadedMethodOrNil (when not nil), since we've aready processed that puppy)"

	oLoadedMethodOrNil
		ifNil: [ 
			"noop"
			^ self ].
	Rowan image
		loadedMethod: oSelector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | 
			oLoadedMethodOrNil ~~ loadedMethod
				ifTrue: [ 
					| theBehavior |
					"we've already updated original theLoadedMethod"
					theBehavior := loadedMethod loadedClass handle.
					isMeta
						ifTrue: [ theBehavior := theBehavior class ].
					(self
						_updateProjectSet: projectSetDefinition
						incomingProject: oProjectDefinition
						incomingPackage: oPackageDefinition
						notification: ex
						theClass: theBehavior
						theLoadedMethod: loadedMethod
						theLoadedProject: loadedMethod loadedProject
						theLoadedPackage: loadedMethod loadedPackage
						originalProjectSet: originalProjectSetDefinition)
						ifNil: [ ^ nil	"resume" ] ]
				ifFalse: [  ] ]
		ifAbsent: [  ].
	^ self
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForScan: oLoadedClassOrNil notification: ex projectDefinition: oProjectDefinition packageDefinition: oPackageDefinition className: className projectSetDefinition: projectSetDefinition originalProjectSet: originalProjectSetDefinition [
	"oLoadedMethod is the original loaded method that kicked off this scan"

	oLoadedClassOrNil
		ifNil: [ 
			"noop"
			^ self ].
	Rowan image
		loadedClassNamed: className
		ifFound: [ :loadedClass | 
			oLoadedClassOrNil ~~ loadedClass
				ifTrue: [ 
					"we have not updated the working project for this class"
					^ self
						_updateProjectSet: projectSetDefinition
						incomingProject: oProjectDefinition
						incomingPackage: oPackageDefinition
						notification: ex
						theClassName: className
						theLoadedClass: loadedClass
						theLoadedProject: loadedClass loadedProject
						theLoadedPackage: loadedClass loadedPackage
						originalProjectSet: originalProjectSetDefinition ]
				ifFalse: [  ] ]
		ifAbsent: [  ]
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

{ #category : 'utilities' }
RwPrjLoadToolV2 >> markProjectSetNotDirty: projectSetDefinition [
	"This method should only be used when the projectSetDefinitions have been read from disk - mark them not dirty. Not dirty means that the loaded things match the code on disk."

	projectSetDefinition deriveLoadedThings
		do: [ :loadedProject | 
			loadedProject markNotDirty.
			loadedProject loadedPackages
				valuesDo: [ :loadedPackage | loadedPackage markNotDirty ] ]
]
