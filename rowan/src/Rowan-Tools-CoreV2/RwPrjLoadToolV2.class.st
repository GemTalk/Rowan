Class {
	#name : 'RwPrjLoadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'private' }
RwPrjLoadToolV2 >> _doProjectSetLoad: projectSetDefinition instanceMigrator: instanceMigrator symbolList: symbolList originalProjectSet: originalProjectSetDefinition processedClassNames: processedClassNames [
	| workingProjectSetDef theOriginalWorkingProjectSetDefinition |
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
					| theClassName incomingPackage theLoadedProject theLoadedMethod |
					theClassName := ex theClass theNonMetaClass name asString.
					incomingPackage := ex incomingPackage.
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
						loadedClassOrMethodDefinition: theLoadedMethod) signal
						ifTrue: [ 
							"incoming wins"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Moving method definition ' , theClassName , '>>'
										, theLoadedMethod handle selector , ' into the package '
										, incomingPackage name.
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
													ex resume ] ] ].	"
							original project set contains a modification to a loaded method that is in a different project"
							workingProjectSetDef := self
								_scanProjectSet: projectSetDefinition
								exMethodNotification: ex
								originalProjectSet: originalProjectSetDefinition ]
						ifFalse: [ 
							"incoming loses, preserve loaded project ownership"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Preserving method definition ' , theClassName , '>>'
										, theLoadedMethod handle selector , ' in the package '
										, theLoadedMethod loadedPackage name.
							workingProjectSetDef := self
								_updateProjectSetForOriginalProjectModification: projectSetDefinition
								exMethodNotification: ex
								className: theClassName
								loadedMethod: theLoadedMethod
								loadedProject: theLoadedProject
								originalProjectSet: originalProjectSetDefinition ] ].
			(ex isKindOf: RwExistingVisitorAddingExistingClassNotification)
				ifTrue: [ 
					| theClassName incomingPackage theLoadedProject |
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
							"incoming wins"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Moving class definition ' , theClassName
										, ' into the package ' , incomingPackage name.
							workingProjectSetDef := self
								_scanProjectSet: projectSetDefinition
								exClassNotification: ex
								originalProjectSet: originalProjectSetDefinition ]
						ifFalse: [ 
							"incoming loses, preserve loaded project ownership"
							Rowan projectTools trace
								trace:
									'[RwPrjLoadToolV2] Preserving class definition ' , theClassName
										, ' in the package ' , ex loadedClass loadedPackage name.
							workingProjectSetDef := self
								_updateProjectSetForOriginalProjectModification: projectSetDefinition
								exClassNotification: ex
								className: theClassName
								loadedProject: theLoadedProject
								originalProjectSet: originalProjectSetDefinition
								processedClassNames: processedClassNames ] ] ].	"trim the stack"
	theOriginalWorkingProjectSetDefinition := workingProjectSetDef
		ifNil: [ originalProjectSetDefinition ]
		ifNotNil: [ 
			"treat the workingProjectSetDef as originalProjectSetDefinition because it contains edits to the originalProjectSetDefinition that must be preserved"
			workingProjectSetDef ].
	^ self
		_doProjectSetLoad: workingProjectSetDef
		instanceMigrator: instanceMigrator
		symbolList: symbolList
		originalProjectSet: theOriginalWorkingProjectSetDefinition
		processedClassNames: processedClassNames
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
RwPrjLoadToolV2 >> _scanFullProjectSetForMoves: workingProjectSetDef loadedMethod: theLoadedMethodOrNil loadedClass: theLoadedClassOrNil originalProjectSet: originalProjectSetDefinition [
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
								projectDefinition: originalProjectDefinition
								packageDefinition: originalPackageDefinition
								className: originalClassName
								projectSetDefinition: workingProjectSetDef
								originalProjectSet: originalProjectSetDefinition ] ] ]
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _scanProjectSet: projectSetDefinition exClassNotification: existingClassNotification originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method that is in a different project"

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
	workingProjectSetDef := self
		_updateProjectSet: projectSetDefinition
		incomingProject: incomingProject
		incomingPackage: incomingPackage
		theClassName: theClassName
		theLoadedClass: theLoadedClass
		theLoadedProject: theLoadedProject
		theLoadedPackage: theLoadedPackage
		originalProjectSet: originalProjectSetDefinition.	"
	while we're in the neighborhood, scan the originalProjectSetDefinition for any loaded 
		method definitions that are not in a different project than the incoming project
		for the method. It's cheaper to do this in one pass, than to use the loader
		to find them. We'll let the loader find class definitions that are in a different project,
		as this happens less frequently."
	self
		_scanFullProjectSetForMoves: workingProjectSetDef
		loadedMethod: nil
		loadedClass: theLoadedClass
		originalProjectSet: originalProjectSetDefinition.

	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _scanProjectSet: projectSetDefinition exMethodNotification: existingMethodNotification originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method that is in a different project"

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
	workingProjectSetDef := self
		_updateProjectSet: projectSetDefinition
		incomingProject: incomingProject
		incomingPackage: incomingPackage
		theClass: theClass
		theLoadedMethod: theLoadedMethod
		theLoadedProject: theLoadedProject
		theLoadedPackage: theLoadedPackage
		originalProjectSet: originalProjectSetDefinition.	"
	while we're in the neighborhood, scan the originalProjectSetDefinition for any loaded 
		method definitions that are not in a different project than the incoming project
		for the method. It's cheaper to do this in one pass, than to use the loader
		to find them. We'll let the loader find class definitions that are in a different project,
		as this happens less frequently."
	self
		_scanFullProjectSetForMoves: workingProjectSetDef
		loadedMethod: theLoadedMethod
		loadedClass: nil
		originalProjectSet: originalProjectSetDefinition.
	^ workingProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSet: projectSetDefinition incomingProject: incomingProject incomingPackage: incomingPackage theClass: theClass theLoadedMethod: theLoadedMethod theLoadedProject: theLoadedProject theLoadedPackage: theLoadedPackage originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method that is in a different project.
		MOVE the definition from the loaded project/package and add it to the incoming project/package"

	| theClasDef originalMethodDefinition theClassName workingPackageDef workingProjectDef workingProjectSetDef |
	theClassName := theClass theNonMetaClass name asString.
	Rowan projectTools trace
		trace:
			'[RwPrjLoadToolV2] Loaded project ' , theLoadedProject name , ' manages method definition of '
				, theClassName
				,
					(theClass isMeta
						ifTrue: [ ' class' ]
						ifFalse: [ '' ]) , '>>' , theLoadedMethod selector , ', incoming project '
				, incomingProject name , ' and package ' , incomingPackage name
				, ' modifies the method.'.	"
	confirm that the definition for theLoadedMethod is in the original project"
	(originalProjectSetDefinition
		projectNamed: incomingProject name
		ifAbsent: [ 
			self
				error:
					'no project named ' , incomingProject name , ' found in original project set.' ])
		ifNotNil: [ :originalProject | 
			| originalPackage originalClassDef |
			originalPackage := originalProject packageNamed: incomingPackage name.
			originalClassDef := originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					originalPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ 
							self
								error:
									'No class definition or extension found for the class ' , theClassName,
					(theClass isMeta
						ifTrue: [ ' class' ]
						ifFalse: [ '' ]) 
										, ' in the incoming package named ' , incomingPackage name
										, ' during load processing.' ] ].
			theClass isMeta
				ifTrue: [ 
					(originalClassDef classMethodDefinitions includesKey: theLoadedMethod selector)
						ifFalse: [ 
							self
								error:
									'No method definition found for the method ' , theClassName , ' class>>'
										, theLoadedMethod selector , ' in the incoming package '
										, incomingPackage name , ' during load processing.' ] ]
				ifFalse: [ 
					(originalClassDef instanceMethodDefinitions
						includesKey: theLoadedMethod selector)
						ifFalse: [ 
							self
								error:
									'No method definition found for the method ' , theClassName , '>>'
										, theLoadedMethod selector , ' in the incoming package '
										, incomingPackage name , ' during load processing.' ] ] ].	"
	confirm that the definition for theLoadedMethod is in the loadedProject project"
	workingProjectSetDef := originalProjectSetDefinition == projectSetDefinition
		ifTrue: [ projectSetDefinition copy ]
		ifFalse: [ 
			"copy was made in an earlier iteration"
			projectSetDefinition ].	"
	a project in the incoming project set is taking ownership of an already  loaded method,
	remove the method from the loaded project's package and attempt a reload"
	workingProjectDef := workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			| projectDef |
			"adding copy of the theloadedProject to the copied project"
			projectDef := theLoadedProject asDefinition.
			workingProjectSetDef addProject: projectDef.
			projectDef ].	"
	confirm that the definition for theLoadedMethod is in the working project"
	(workingProjectSetDef
		projectNamed: theLoadedProject name
		ifAbsent: [ 
			self
				error:
					'no project named ' , theLoadedProject name
						, ' found in working projectSet project set.' ])
		ifNotNil: [ :originalProject | 
			| originalPackage originalClassDef |
			originalPackage := originalProject packageNamed: theLoadedPackage name.
			originalClassDef := originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					originalPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ 
							self
								error:
									'No class definition or extension found for the class ' , theClassName
										, ' in the working package named ' , theLoadedProject name
										, ' during load processing.' ] ].
			theClass isMeta
				ifTrue: [ 
					(originalClassDef classMethodDefinitions includesKey: theLoadedMethod selector)
						ifFalse: [ 
							self
								error:
									'No method definition found for the method ' , theClassName , ' class>>'
										, theLoadedMethod selector , ' in the working package '
										, theLoadedPackage name , ' during load processing.' ] ]
				ifFalse: [ 
					(originalClassDef instanceMethodDefinitions
						includesKey: theLoadedMethod selector)
						ifFalse: [ 
							self
								error:
									'No method definition found for the method ' , theClassName , '>>'
										, theLoadedMethod selector , ' in the working package '
										, theLoadedPackage name , ' during load processing.' ] ] ].
	workingPackageDef := workingProjectDef packageNamed: theLoadedPackage name.
	theClasDef := workingPackageDef
		classExtensionDefinitionNamed: theClassName
		ifAbsent: [ workingPackageDef classDefinitionNamed: theClassName ]."
	Remove the method definition from the working project ... it is already 
		present in the incoming package, so no need for explicit add. "
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
RwPrjLoadToolV2 >> _updateProjectSet: projectSetDefinition incomingProject: incomingProject incomingPackage: incomingPackage theClassName: theClassName theLoadedClass: theLoadedClass theLoadedProject: theLoadedProject theLoadedPackage: theLoadedPackage originalProjectSet: originalProjectSetDefinition [
	"original project set contains a modification to a method or class that is in a different project.
		MOVE the definition from the loaded project/package and add it to the incoming project/package"

	| workingProjectSetDef workingProjectDef workingPackageDef originalClassDef |
	Rowan projectTools trace
		trace:
			'[RwPrjLoadToolV2] Loaded project ' , theLoadedProject name
				, ' manages the class definition of ' , theClassName , ', incoming project '
				, incomingProject name , ' and package ' , incomingPackage name
				, ' modifies the class.'.	"
	confirm that the definition for theLoadedClass is in the original project"
	(originalProjectSetDefinition
		projectNamed: incomingProject name
		ifAbsent: [ 
			self
				error:
					'no project named ' , incomingProject name , ' found in original project set.' ])
		ifNotNil: [ :originalProject | 
			| originalPackage |
			originalPackage := originalProject packageNamed: incomingPackage name.
			originalPackage
				classDefinitionNamed: theClassName
				ifAbsent: [ 
					originalPackage
						classExtensionDefinitionNamed: theClassName
						ifAbsent: [ 
							self
								error:
									'No class definition found for the class ' , theClassName , incomingPackage name
										, ' during load processing.' ] ] ].	"
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
	workingPackageDef := workingProjectDef packageNamed: theLoadedPackage name."
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
RwPrjLoadToolV2 >> _updateProjectSetForOriginalProjectModification: projectSetDefinition exClassNotification: existingClassNotification className: theClassName loadedProject: theLoadedProject originalProjectSet: originalProjectSet processedClassNames: processedClassNames [
	| copiedProjectSetDef projectDef theClass loadedClass packageDef |
	theClass := existingClassNotification theClass.
	copiedProjectSetDef := projectSetDefinition copy.	"
	a project in the original project set is taking ownership of an already  loaded class,
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
	processedClassNames add: theClassName.
	^ copiedProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForOriginalProjectModification: projectSetDefinition exMethodNotification: ex className: theClassName loadedMethod: theLoadedMethod loadedProject: theLoadedProject originalProjectSet: originalProjectSet [
	"the loaded project wins, so modify and return a copy of the projectSetDefinition with the conflicting method definition removed"

	| theClasDef originalMethodDefinition copiedProjectSetDef projectDef packageDef |
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
						, ' ) in project ' , theLoadedProject name printString , ' and project '
						, ex incomingProject name printString ].
	^ copiedProjectSetDef
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForScan: oLoadedMethod projectDefinition: oProjectDefinition packageDefinition: oPackageDefinition className: className isMeta: isMeta selector: oSelector methodDefinition: oMethodDefinition projectSet: projectSetDefinition originalProjectSet: originalProjectSetDefinition [
	"oLoadedMethod is the original loaded method that kicked off this scan"

	Rowan image
		loadedMethod: oSelector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | 
			oLoadedMethod ~~ loadedMethod
				ifTrue: [ 
					| theBehavior |
					"we've already updated original theLoadedMethod"
					theBehavior := loadedMethod loadedClass handle.
					isMeta
						ifTrue: [ theBehavior := theBehavior class ].
					^ self
						_updateProjectSet: projectSetDefinition
						incomingProject: oProjectDefinition
						incomingPackage: oPackageDefinition
						theClass: theBehavior
						theLoadedMethod: loadedMethod
						theLoadedProject: loadedMethod loadedProject
						theLoadedPackage: loadedMethod loadedPackage
						originalProjectSet: originalProjectSetDefinition ]
				ifFalse: [  ] ]
		ifAbsent: [  ].
	^ projectSetDefinition
]

{ #category : 'private' }
RwPrjLoadToolV2 >> _updateProjectSetForScan: oLoadedClass projectDefinition: oProjectDefinition packageDefinition: oPackageDefinition className: className projectSetDefinition: projectSetDefinition originalProjectSet: originalProjectSetDefinition [
	"oLoadedMethod is the original loaded method that kicked off this scan"

	Rowan image
		loadedClassNamed: className
		ifFound: [ :loadedClass | 
			oLoadedClass ~~ loadedClass
				ifTrue: [ 
					"we have not updated the working project for this class"
					^ self
						_updateProjectSet: projectSetDefinition
						incomingProject: oProjectDefinition
						incomingPackage: oPackageDefinition
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
