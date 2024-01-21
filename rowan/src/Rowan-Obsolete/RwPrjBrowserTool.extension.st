Extension { #name : 'RwPrjBrowserTool' }

{ #category : '*rowan-obsolete' }
RwPrjBrowserTool >> addOrUpdateMethod: methodSource inProtocol: hybridPackageName forClassNamed: className isMeta: isMeta [
	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	"a hybrid package name has a leading '*' followed by the name of a package ... 
 		where the hybrid package name is not expected to preserve case"

	"this method is only needed for the transition from Oscar 3.x to Oscar 4.0 (Rowan V2 aware Jadeite"

	| methodDef loadedProject loadedPackage couldBeHybrid hybridLoadedPackage hybridLoadedProject loadedClass |
	couldBeHybrid := (hybridPackageName at: 1) = $*.
	couldBeHybrid
		ifTrue: [ 
			hybridLoadedPackage := Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [  ].
			hybridLoadedPackage
				ifNotNil: [ 
					| pkgConvention |
					hybridLoadedProject := hybridLoadedPackage loadedProject.
					pkgConvention := hybridLoadedProject packageConvention.
					(pkgConvention = 'RowanHybrid' or: [ pkgConvention = 'Monticello' ])
						ifTrue: [ 
							| classPackageName "everything is cool" |
							classPackageName := (Rowan image objectNamed: className) rowanPackageName.
							classPackageName = hybridLoadedPackage name
								ifTrue: [ 
									"https://github.com/GemTalk/Rowan/issues/802"
									self
										error:
											'extension category name ' , hybridPackageName printString
												, '  must not match class package name (' , classPackageName , ')' ] ]
						ifFalse: [ 
							"the project associated with the hybridPackageName _is NOT_ using the `RowanHybrid` package convention - questionable use of hybrid convention in a non-hybrid project"
							Warning
								signal:
									'The package convention for this project ('
										, hybridLoadedProject name printString
										,
											') is ''Rowan'' and a leading $* in the category is not used to denote a target package for any methods placed in this category'.
							couldBeHybrid := false ] ] ].
	methodDef := RwMethodDefinition
		newForSource: methodSource
		protocol: hybridPackageName.
	(self
		_loadedMethod: methodDef selector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [  ])
		ifNil: [ 
			"new method extract project information from the class"
			(Rowan image loadedClassNamed: className ifAbsent: [  ])
				ifNil: [ 
					"unpackaged class?"
					couldBeHybrid
						ifTrue: [ 
							hybridLoadedPackage
								ifNil: [ 
									self
										error:
											'A package for package name ' , hybridPackageName printString
												, ' was not found.' ].
							hybridLoadedProject
								ifNil: [ 
									self
										error:
											'Attempt to add a method to an unpackaged class ' , className printString
												, ', while using `hybrid-style` method protocol '
												, hybridPackageName printString
												,
													' for a project that does not use the `RowanHybrid` package convention.' ].
							loadedPackage := hybridLoadedPackage.
							loadedProject := hybridLoadedProject ]
						ifFalse: [ 
							| theBehavior |
							"Adding unpackaged method to an unpackaged class - if permitted"
							theBehavior := Rowan image objectNamed: className.
							isMeta
								ifTrue: [ theBehavior := theBehavior class ].
							RwPerformingUnpackagedEditNotification
								signal:
									'Attempt to add or modify an unpackage method in the class '
										, className printString
										, '. The modification will not be tracked by Rowan'.
							^ theBehavior
								compileMethod: methodSource
								dictionaries: Rowan image symbolList
								category: hybridPackageName
								environmentId: 0	"Notification resumed, so continue with add/modify" ] ]
				ifNotNil: [ :theLoadedClass | 
					loadedClass := theLoadedClass.
					couldBeHybrid
						ifTrue: [ 
							hybridLoadedPackage
								ifNil: [ 
									self
										error:
											'A package for package name ' , hybridPackageName printString
												, ' was not found.' ].
							hybridLoadedProject
								ifNil: [ 
									self
										error:
											'Attempt to add a method to an unpackaged class ' , className printString
												, ', while using `hybrid-style` method protocol '
												, hybridPackageName printString ].
							loadedPackage := hybridLoadedPackage.
							loadedProject := hybridLoadedProject ]
						ifFalse: [ 
							"new method for packaged class, so add method to the class' package"
							loadedProject := theLoadedClass loadedProject.
							loadedPackage := theLoadedClass loadedPackage ] ] ]
		ifNotNil: [ :loadedMethod | 
			| isHybrid |
			"change to existing loaded method - keep in mind that the method could be being moved between packages via protocol change"
			loadedProject := loadedMethod loadedProject.
			loadedClass := loadedMethod loadedClass.
			isHybrid := loadedProject packageConvention = 'RowanHybrid'.
			couldBeHybrid
				ifTrue: [ 
					"protocol has leading *"
					hybridLoadedPackage
						ifNil: [ 
							"questionable use of hybrid protocol in a non-RowanHybrid project, but legal"
							loadedPackage := loadedMethod loadedPackage ]
						ifNotNil: [ 
							isHybrid
								ifTrue: [ 
									"the current project for the method is using the `RowanHybrid` package convention"
									hybridLoadedProject
										ifNil: [ 
											"VERY questionable use of hybrid protocol, package matching the hyybrid protocol was found, but the project of the package is not using `RowanHybrid` package convention, while the current project _IS_ using hybrid protocol --- ILLEGAL"
											self
												error:
													'Attempt to use RowanHybrid convention ' , hybridPackageName printString
														, ' for a package ' , hybridLoadedPackage name printString
														, ' that belongs to a project '
														, hybridLoadedPackage loadedProject name printString
														, '  that is not using `RowanHybrid` package convention.' ]
										ifNotNil: [ 
											"moving from one hybrid package to another (or same) hybrid package"
											loadedPackage := hybridLoadedPackage.
											loadedProject := hybridLoadedProject ] ]
								ifFalse: [ 
									"the current project is NOT using the `RowanHybrid` package convention"
									hybridLoadedProject
										ifNil: [ 
											"Questionable use of hybrid protocol, package matching the hyybrid protocol was found, but the project of the package is not using `RowanHybrid` package convention"
											"USE THE CURRENT PACKAGE AND PROJECT"
											loadedPackage := loadedMethod loadedPackage ]
										ifNotNil: [ 
											"moving from current package to a hybrid project and package"
											loadedPackage := hybridLoadedPackage.
											loadedProject := hybridLoadedProject ] ] ] ]
				ifFalse: [ 
					"use the existing package for method"
					loadedPackage := loadedMethod loadedPackage ] ].

	loadedPackage loadedProject == loadedProject
		ifFalse: [ 
			self
				error:
					'internal error - the expected loaded project ' , loadedProject name printString
						, ' does not match the actual loaded project '
						, loadedPackage loadedProject name printString , ' of the package '
						, loadedPackage name printString , ' for the method ' , loadedClass name
						,
							(isMeta
								ifTrue: [ ' class ' ]
								ifFalse: [ '' ]) , '>>' , methodDef selector ].

	^ self
		addOrUpdateMethodDefinition: methodDef
		forClassNamed: className
		isMeta: isMeta
		inLoadedPackage: loadedPackage
]

{ #category : '*rowan-obsolete' }
RwPrjBrowserTool >> addOrUpdateMethodDefinition: methodDef forClassNamed: className isMeta: isMeta inLoadedPackage: loadedPackage [
	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	| projectTools classExtensionDef updateBlock projectDefinition packageDefinition projectSetDefinition loadedMethodToBeRemoved |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedMethodToBeRemoved := self
		_loadedMethod: methodDef selector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			"no pre-existing method for this selector installed"
			 ].

	projectTools := Rowan projectTools.
	updateBlock := [ :cDef :pDef | 
	loadedMethodToBeRemoved
		ifNil: [ 
			"no method needs to be remove, just add the method to the class or extension def"
			isMeta
				ifTrue: [ cDef addClassMethodDefinition: methodDef ]
				ifFalse: [ cDef addInstanceMethodDefinition: methodDef ] ]
		ifNotNil: [ :loadedMethod | 
			| loadedPackageForMethod |
			loadedPackageForMethod := loadedMethod loadedPackage.
			loadedPackageForMethod == loadedPackage
				ifTrue: [ 
					"loaded method being updated in same package, sjust update the method def"
					isMeta
						ifTrue: [ cDef updateClassMethodDefinition: methodDef ]
						ifFalse: [ cDef updateInstanceMethodDefinition: methodDef ] ]
				ifFalse: [ 
					| loadedClassOrExtension projectDef packageDef crDef |
					"loaded method in different package than new version of method"
					projectDef := loadedPackageForMethod loadedProject asDefinition.
					projectDef name = pDef name
						ifTrue: [ 
							"both packages are in same project"
							projectDef := pDef ]
						ifFalse: [ 
							"each package in a different project, will need to load loaded method project as well"
							projectSetDefinition addProject: projectDef ].
					packageDef := projectDef packageNamed: loadedPackageForMethod name.
					loadedClassOrExtension := loadedMethod loadedClass.
					crDef := loadedClassOrExtension isLoadedClass
						ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
						ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].	"remove the method from one package and add it to the other"
					isMeta
						ifTrue: [ 
							crDef removeClassMethod: methodDef selector.
							cDef addClassMethodDefinition: methodDef ]
						ifFalse: [ 
							crDef removeInstanceMethod: methodDef selector.
							cDef addInstanceMethodDefinition: methodDef ] ] ].
	projectSetDefinition addProject: pDef.
	projectTools load loadProjectSetDefinition: projectSetDefinition.
	(self _loadedMethod: methodDef selector inClassNamed: className isMeta: isMeta)
		handle ].

	self
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			packageDef name = loadedPackage name
				ifTrue: [ ^ updateBlock value: classDef value: projectDef ]
				ifFalse: [ 
					"the named package is different from the class definition package"
					 ] ]
		ifAbsent: [ 
			"no loaded class definition, so we probably need to add a class extension"
			 ].
	projectDefinition := loadedPackage loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: loadedPackage name.

	classExtensionDef := packageDefinition classExtensions
		at: className
		ifAbsent: [ 
			"no existing class extension definition ... create a new one"
			classExtensionDef := RwClassExtensionDefinition newForClassNamed: className.

			packageDefinition addClassExtensionDefinition: classExtensionDef.
			classExtensionDef ].

	^ updateBlock value: classExtensionDef value: projectDefinition
]
