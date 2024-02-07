Class {
	#name : 'RwPrjBrowserToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Obsolete'
}

{ #category : 'private' }
RwPrjBrowserToolV2 >> _copyClassDefinitionNamed: className to: newName [

	"answer a projectSetDefinition that includes the copied class definition"

	| loadedClass classDef loadedPackage projectDef packageDef  projectSetDefinition |
	(self _loadedClassNamed: newName ifAbsent:  [])
		ifNotNil: [ self error: 'There is already a class named ', newName printString, ' in the system.' ].
	loadedClass := self 
		_loadedClassNamed: className 
		ifAbsent: [  self error: 'No loaded class named: ', className printString , ' found.' ].
	projectSetDefinition := RwProjectSetDefinition new.
	loadedPackage := loadedClass loadedPackage.
	projectDef := loadedPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	classDef := loadedClass asDefinition.
	classDef name: newName.
	packageDef addClassDefinition: classDef.
	projectSetDefinition addProject: projectDef.

	(self _loadedClassExtensionsNamed: className ifAbsent: [ #() ])
		do: [:loadedClassExtension | 
			| classExtDef loadedProject |
			loadedPackage := loadedClassExtension loadedPackage.
			loadedProject := loadedPackage loadedProject.
			projectDef := projectSetDefinition
				projectNamed: loadedPackage loadedProject name
				ifAbsent: [ 
					projectDef := loadedProject asDefinition.
					projectSetDefinition addProject: projectDef.
					projectDef ].

			packageDef := projectDef packageNamed: loadedPackage name.
			classExtDef := loadedClassExtension asDefinition.
			classExtDef name: newName.
			packageDef addClassExtensionDefinition: classExtDef ].

	^ projectSetDefinition
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedClassExtensionsNamed: className ifAbsent: absentBlock [

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | ^ loadedClassExtensionSet ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedClassNamed: className ifAbsent: absentBlock [

	^ Rowan image loadedClassNamed: className ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta [

	^ self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			self
				error:
					'No loaded method for ' , methodSelector printString , ' was found for class '
						, className printString ]
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta ifAbsent: absentBlock [

	^ Rowan image
		loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | ^ loadedMethod ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedPackageNamed: packageName [

	^ self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ]
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedPackageNamed: packageName ifAbsent: absentBlock [

	^ Rowan image loadedPackageNamed: packageName ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _loadedProjectNamed: projectName [

	^ Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self error: 'No project named ' , projectName printString , ' found' ]
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _projectNamed: projectName [

	^ (self _loadedProjectNamed: projectName) asDefinition
]

{ #category : 'private' }
RwPrjBrowserToolV2 >> _rowanSymbolDictionaryNames [

	^ #( #RowanKernel #RowanLoader #RowanTools )

]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category options: optionsArray [
	| loadedPackage packageConvention packageName |
	(self _loadedClassNamed: className ifAbsent: [  ])
		ifNil: [ 
			"no loaded class, see if the category matches an existing package name"
			loadedPackage := self
				_loadedPackageNamed: category
				ifAbsent: [ self error: 'No package named ' , category printString , ' found' ] ]
		ifNotNil: [ :loadedClass | loadedPackage := loadedClass loadedPackage ].
	packageConvention := loadedPackage loadedProject packageConvention.
	packageConvention = 'Rowan'
		ifTrue: [ packageName := loadedPackage name ]
		ifFalse: [ 
			packageConvention = 'RowanHybrid'
				ifTrue: [ 
					packageName := (loadedPackage loadedProject loadedPackages
						at: category
						ifAbsent: [ 
							self
								error:
									'No package named ' , category printString
										, ' found (RowanHybrid package convention)' ]) name ]
				ifFalse: [ 
					"Monticello"
					packageName := (loadedPackage loadedProject loadedPackages
						detect: [ :each | category beginsWith: each name ]
						ifNone: [ 
							self
								error:
									'No package matching ' , category printString
										, ' found (Monticello package convention)' ]) name ] ].
	^ self
		addOrUpdateClassDefinition: className
		type: type
		superclass: superclassName
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: category
		packageName: packageName
		constraints: #()
		options: optionsArray
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName constraints: constraintsArray options: optionsArray [
	| loadedPackage loadedProject loadedClass theOptions theConstraints |
	loadedPackage := self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	loadedProject := loadedPackage loadedProject.
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	loadedClass := self _loadedClassNamed: className ifAbsent: [  ].
	theOptions := optionsArray collect: [ :each | each asString ].
	(constraintsArray isKindOf: Array)
		ifTrue: [ 
			theConstraints := {}.
			constraintsArray
				do: [ :ar | 
					(ar isKindOf: Array)
						ifTrue: [ 
							theConstraints
								add:
									{((ar at: 1) asString).
									((ar at: 2) name asString)} ]
						ifFalse: [ theConstraints add: ar name asString ] ] ]
		ifFalse: [ theConstraints := constraintsArray ifNil: [ #() ] ifNotNil: [ constraintsArray ] ].
	(Rowan image validClassCategory: category forLoadedPackage: loadedPackage)
		ifFalse: [ 
			self
				error:
					'Category ' , category printString , ' for class ' , className printString
						, 'does not follow ' , loadedProject packageConvention
						, ' package convention' ].
	loadedClass
		ifNil: [ 
			| classDef |
			"create a new class"
			classDef := RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: (anArrayOfStrings collect: [ :each | each asString ])
				classinstvars: (anArrayOfClassInstVars collect: [ :each | each asString ])
				classvars: (anArrayOfClassVars collect: [ :each | each asString ])
				category: category
				comment: nil
				pools: #()
				type: type.
			classDef gs_options: theOptions.
			classDef gs_constraints: theConstraints.
			self createClass: classDef inPackageNamed: packageName ]
		ifNotNil: [ 
			self class edit
				definitionsForClassNamed: className
				ifFound: [ :classDef :packageDef :projectDef | 
					| classProjectDef projectSetDef |
					classProjectDef := projectDef.
					projectSetDef := RwProjectSetDefinition new.
					projectSetDef addProject: classProjectDef.
					classDef
						superclassName: superclassName;
						instVarNames: (anArrayOfStrings collect: [ :each | each asString ]);
						classVarNames: (anArrayOfClassVars collect: [ :each | each asString ]);
						classInstVarNames:
								(anArrayOfClassInstVars collect: [ :each | each asString ]);
						gs_options: theOptions;
						gs_constraints: theConstraints;
						category: category;
						yourself.
					loadedClass packageName = packageName
						ifFalse: [ 
							| thePackageDef |
							"move class to another package"
							packageDef removeClassDefinition: classDef.
							thePackageDef := classProjectDef
								packageNamed: packageName
								ifAbsent: [ 
									| packageProjectDef |
									"loadedPackage in a different project"
									packageProjectDef := loadedPackage loadedProject asDefinition.
									projectSetDef addProject: packageProjectDef.
									packageProjectDef packageNamed: packageName ].
							thePackageDef addClassDefinition: classDef ].
					self class load loadProjectSetDefinition: projectSetDef ]
				ifAbsent: [ 
					"shouldn't happen ... loadedClass known to exist"
					self error: 'unexpected missing class ' , className printString ] ].
	^ (self _loadedClassNamed: className ifAbsent: [  ]) handle currentVersion
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName options: optionsArray [

	^ self 
		addOrUpdateClassDefinition: className
		type: type 
		superclass: superclassName 
		instVarNames: anArrayOfStrings 
		classVars: anArrayOfClassVars 
		classInstVars: anArrayOfClassInstVars 
		poolDictionaries: anArrayOfPoolDicts 
		category: category 
		packageName: packageName 
		constraints: #() 
		options: optionsArray

]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> addOrUpdateMethod: methodSource dictionaries: aSymbolList inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	| projectTools loadedPackage classExtensionDef methodDef updateBlock projectDefinition packageDefinition projectSetDefinition loadedMethodToBeRemoved packageConvention |
	projectSetDefinition := RwProjectSetDefinition new.

	methodDef := RwMethodDefinition newForSource: methodSource protocol: protocol.
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
			loadedPackageForMethod name = packageName
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
	projectTools load
		loadProjectSetDefinition: projectSetDefinition
		symbolList: aSymbolList.
	(self _loadedMethod: methodDef selector inClassNamed: className isMeta: isMeta)
		handle ].

	self
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName
				ifTrue: [ 
					packageConvention := projectDef packageConvention.
					packageConvention = 'Rowan'
						ifTrue: [ 
							"method protocol is not restricted"
							 ]
						ifFalse: [ 
							packageConvention = 'RowanHybrid'
								ifTrue: [ 
									(protocol equalsNoCase: '*' , packageName)
										ifTrue: [ 
											self
												error:
													'The supplied method protocol ' , protocol printString
														,
															' does not follow the expected package convention for ''RowanHybrid''. The protocol indicates an extension method for the package in which the class resides, which is not an extension method' ] ]
								ifFalse: [ self error: 'Unsupported packageConvention ' , packageConvention printString ] ].
					^ updateBlock value: classDef value: projectDef ]
				ifFalse: [ 
					"the named package is different from the class definition package"
					 ] ]
		ifAbsent: [ 
			"no loaded class definition, so we probably need to add a class extension"
			 ].
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'A package named ' , packageName printString , ' was not found.' ].

	projectDefinition := loadedPackage loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: packageName.

	packageConvention := projectDefinition packageConvention.
	packageConvention = 'Rowan'
		ifTrue: [ 
			"method protocol is not restricted"
			 ]
		ifFalse: [ 
			packageConvention = 'RowanHybrid'
				ifTrue: [ 
					| expectedProtocol |
					expectedProtocol := loadedPackage asExtensionName.
					(expectedProtocol equalsNoCase: protocol)
						ifFalse: [ 
							self
								error:
									'The supplied method protocol ' , protocol printString
										, ' does not follow the expected package convention '
										, expectedProtocol printString , ' for ''RowanHybrid''' ] ]
				ifFalse: [ self error: 'Unsupported packageConvention ' , packageConvention printString ] ].

	classExtensionDef := packageDefinition classExtensions
		at: className
		ifAbsent: [ 
			"no existing class extension definition ... create a new one"
			classExtensionDef := RwClassExtensionDefinition newForClassNamed: className.

			packageDefinition addClassExtensionDefinition: classExtensionDef.
			classExtensionDef ].

	^ updateBlock value: classExtensionDef value: projectDefinition
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> addOrUpdateMethod: methodSource inProtocol: hybridPackageName forClassNamed: className isMeta: isMeta [
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

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> addOrUpdateMethod: methodSource inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	^ self
		addOrUpdateMethod: methodSource
		dictionaries: Rowan image symbolList
		inProtocol: protocol
		forClassNamed: className
		isMeta: isMeta
		inPackageNamed: packageName
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> addOrUpdateMethodDefinition: methodDef forClassNamed: className isMeta: isMeta inLoadedPackage: loadedPackage [
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

{ #category : 'package browsing' }
RwPrjBrowserToolV2 >> addPackageNamed: packageName toComponentNamed: componentName andProjectNamed: projectName [
	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	projectDefinition addPackageNamed: packageName toComponentNamed: componentName.
	projectDefinition load
]

{ #category : 'package browsing' }
RwPrjBrowserToolV2 >> addPackageNamed: packageName toProjectNamed: projectName [

	^ self addPackageNamed: packageName toComponentNamed: 'Core' andProjectNamed: projectName
]

{ #category : 'project browsing' }
RwPrjBrowserToolV2 >> addRowanSymbolDictionariesToPersistentSymbolList [

	self addRowanSymbolDictionariesToPersistentSymbolListFor: System myUserProfile

]

{ #category : 'project browsing' }
RwPrjBrowserToolV2 >> addRowanSymbolDictionariesToPersistentSymbolListFor: userProfile [

	| systemUser |
	systemUser := AllUsers userWithId: 'SystemUser'.
	self _rowanSymbolDictionaryNames do: [:symDictName |
		| aSymbolDictionary anIndex |
		aSymbolDictionary := systemUser objectNamed: symDictName.
		anIndex := userProfile symbolList size + 1.
		userProfile
			insertDictionary: aSymbolDictionary at: anIndex ]

]

{ #category : 'project browsing' }
RwPrjBrowserToolV2 >> addRowanSymbolDictionariesToTransientSymbolList [
  | systemUser |
  systemUser := AllUsers userWithId: 'SystemUser'.
  self _rowanSymbolDictionaryNames
    do: [ :symDictName | 
      | aSymbolDictionary anIndex transient |
      transient := GsSession currentSession symbolList.
      aSymbolDictionary := systemUser objectNamed: symDictName.
      anIndex := transient size + 1.
      transient insertObject: aSymbolDictionary at: anIndex ]
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForClass: aClass [

	^ self classCreationTemplateForClass: aClass hybridBrowser: false
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser [

	| result anArray lfsp newByteSubclass civs superClass className thePackageName nonRowanClass |
	result := String new.
	superClass := aClass superclass.
	className := aClass name asString.
	superClass
		ifNil: [ result addAll: 'nil' ]
		ifNotNil: [ result addAll: superClass name asString ].
	lfsp := Character lf asString tab.
	newByteSubclass := false.
	thePackageName := aClass rowanPackageName.
	nonRowanClass := thePackageName = Rowan unpackagedName.
	(aClass isBytes _and: [ superClass isBytes not ])
		ifTrue: [ 
			nonRowanClass
				ifTrue: [ result addAll: ' byteSubclass: ''' ]
				ifFalse: [ result addAll: ' rwByteSubclass: ''' ].
			result
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			(aClass isIndexable and: [superClass isIndexable not])
				ifTrue: [ 
					nonRowanClass
						ifTrue: [ result addAll: ' indexableSubclass: ''' ]
						ifFalse: [ result addAll: ' rwIndexableSubclass: ''' ].
					result
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					nonRowanClass
						ifTrue: [ result addAll: ' subclass: ''' ]
						ifFalse: [ result addAll: ' rwSubclass: ''' ].
					result
						addAll: className;
						addLast: $' ] ].
	newByteSubclass
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(';
				addAll: (aClass _instVarNamesWithSeparator: lfsp , '                 ');
				add: $) ].
	result
		addAll: lfsp;
		addLast: 'classVars: #('.
	aClass _sortedClassVarNames
		do: [ :aKey | 
			result addLast: $ .
			(aKey includesValue: $')
				ifTrue: [ result addAll: aKey _asSource ]
				ifFalse: [ result addAll: aKey ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addLast: 'classInstVars: #('.
	civs := aClass class allInstVarNames.
	civs removeFrom: 1 to: aClass class superClass instSize.
	civs
		do: [ :civName | 
			result addLast: $ .
			(civName includesValue: $')
				ifTrue: [ result addAll: civName _asSource ]
				ifFalse: [ result addAll: civName ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addAll: 'poolDictionaries: '.
	result addAll: '#()'.	"ignored for now"
	nonRowanClass
		ifTrue: [ 
			"if the class is unpackaged, then we need to provide for the specification of symbol dictionary into which the class will be installed"
			result
				addAll: lfsp;
				addAll: 'inDictionary: '.
			anArray := Rowan image symbolList dictionariesAndSymbolsOf: aClass.
			anArray isEmpty
				ifTrue: [ result addAll: '''''' ]
				ifFalse: [ result addAll: ((anArray at: 1) at: 1) name asString ] ]
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'category: '.
			result addAll: aClass category printString.
			(hybridBrowser and: [ thePackageName = aClass category ])
				ifFalse: [ 
					result
						addAll: lfsp;
						addAll: 'packageName: '.
					result addAll: thePackageName printString ] ].
	aClass _hasConstraints
		ifTrue: [ 
			result
				add: lfsp;
				add: aClass _rwDefinitionOfConstraints ].
	result
		add: lfsp;
		add: aClass _rwOptionsForDefinition.
	result add: Character lf.
	^ result

]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForSubclassOf: superclassName category: category [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		category: category
		packageName: nil
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: 'NameOfSubclass'
		category: category
		packageName: packageName
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForSubclassOf: superclassName className: className category: category [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: className
		category: category
		packageName: nil
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForSubclassOf: superclassName className: className category: category packageName: packageName [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: className
		type: 'normal'
		category: category
		packageName: packageName
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForSubclassOf: superclassName className: className type: classType category: category packageName: packageNameOrNil [

	"classType may be 'normal', 'byteSubclass', or 'variable'"

	| result lfsp superClass |
	result := String new.
	superClass := Rowan image objectNamed: superclassName.
	result addAll: superclassName.
	lfsp := Character lf asString tab.
	classType = 'normal'
		ifTrue: [ result addAll: ' rwSubclass: ' ].
	classType = 'variable'
		ifTrue: [ result addAll: ' rwIndexableSubclass: ' ].
	classType = 'byteSubclass'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className printString.
	classType ~= 'byteSubclass'
		ifTrue: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #()' ].
	result
		addAll: lfsp;
		addLast: 'classVars: #()';
		addAll: lfsp;
		addLast: 'classInstVars: #()';
		addAll: lfsp;
		addAll: 'poolDictionaries: #()'.
	result
		addAll: lfsp;
		addAll: 'category: ';
		addAll: category printString.
	packageNameOrNil
		ifNotNil: [ 
			result
				addAll: lfsp;
				addAll: 'packageName: '.
			result addAll: packageNameOrNil printString ].
	result
		add: lfsp;
		add: 'options: #()';
		add: Character lf.
	^ result
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> classCreationTemplateForSubclassOf: superclassName className: className type: classType instanceVariablesString: instanceVariablesString classVariablesString: classVariablesString classInstanceVariablesString: classInstanceVariablesString poolDictionariesString: poolDictionariesString 
comment: comment category: category [

	"classType may be 'normal', 'byteSubclass', or 'variable'"

	| result lfsp |
	result := String new.
	lfsp := Character lf asString tab.
	comment isEmpty 
		ifFalse: [ 
			result 
				addAll: '(';
				addAll: lfsp ].
	result
		addAll: superclassName.
	classType = 'normal'
		ifTrue: [ result addAll: ' rwSubclass: ' ].
	classType = 'variable'
		ifTrue: [ result addAll: ' rwIndexableSubclass: ' ].
	classType = 'byteSubclass'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className.
	classType ~= 'byteSubclass'
		ifTrue: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(' , instanceVariablesString , ')' ].
	result
		addAll: lfsp;
		addLast: 'classVars: #(' , classVariablesString , ')';
		addAll: lfsp;
		addLast: 'classInstVars: #(' , classInstanceVariablesString , ')';
		addAll: lfsp;
		addAll: 'poolDictionaries: #(' , poolDictionariesString , ')'.
	result
		addAll: lfsp;
		addAll: 'category: ';
		addAll: category.
	result
		add: lfsp;
		add: 'options: #()';
		add: Character lf.
	comment isEmpty 
		ifFalse: [ 
			result 
				addAll: ') ';
				addAll: lfsp;
				addAll: 'comment: ';
				addAll: comment;
				addAll: lfsp ].
	^ result
]

{ #category : 'definition updating' }
RwPrjBrowserToolV2 >> classNamed: className updateDefinition: updateBlock [

	^ self
		classNamed: className
		updateDefinition: updateBlock
		ifAbsent: [ 
			self
				error:
					'expected class definition for class ' , className printString , ' not found.' ]
]

{ #category : 'definition updating' }
RwPrjBrowserToolV2 >> classNamed: className updateDefinition: updateBlock ifAbsent: absentBlock [

	| projectDefinition classDefinition |
	self class edit
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			projectDefinition := projectDef.
			classDefinition := classDef ]
		ifAbsent: [ ^ absentBlock value ].

	updateBlock value: classDefinition.
	projectDefinition load
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> copyClassNamed: className to: newName [

	"anser the new copy of the class"

	| projectSetDefinition |
	projectSetDefinition := self _copyClassDefinitionNamed: className to: newName.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	^ Rowan globalNamed: newName
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> createClass: classDefinition inPackageNamed: packageName [
	| loadedPackage projectDefinition |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package named ' , packageName printString , ' was not found' ].
	projectDefinition := loadedPackage loadedProject asDefinition.

	Rowan projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectDefinition load
]

{ #category : 'project browsing' }
RwPrjBrowserToolV2 >> exposeRowanToolsSymbolDictionary [

	| transient size dict |
	transient := Rowan image symbolList.
	size := transient size.
	dict := (AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools'.
	transient insertObject: dict at: size + 1.

]

{ #category : 'modification short cuts' }
RwPrjBrowserToolV2 >> forceRecompileLoadedMethod: loadedMethod dictionaries: symbolList forClassNamed: className isMeta: isMeta [
	"direct creation of the modifications needed to force recompile of the method "

	"https://github.com/GemTalk/Rowan/issues/893"

	| methodDef source methodModification loadedClass classDef classModification instanceMethodsModification classMethodsModification methodModificationClass loadedPackage packageDef packageModification classesModification loadedProject projectDef projectModification packagesModification sourceModification projectSetModification |
	methodDef := loadedMethod asDefinition.
	loadedClass := loadedMethod loadedClass.
	classDef := loadedClass asDefinition.

	methodModificationClass := loadedClass isLoadedClassExtension
		ifTrue: [ RwExtensionMethodModification ]
		ifFalse: [ RwMethodModification ].
	methodModification := methodModificationClass
		before: methodDef
		after: methodDef.
	methodModification
		classDefinition: classDef;
		isMeta: isMeta.

	source := methodDef source.
	sourceModification := RwSourceModification new
		_addElementModification:
				(RwPropertyModification key: 'source' oldValue: source newValue: source);
		yourself.
	methodModification
		propertiesModification: RwPropertiesModification new;
		sourceModification: sourceModification.

	classModification := classDef _modificationClass
		before: classDef
		after: classDef.
	classModification
		propertiesModification: RwPropertiesModification new;
		yourself.
	classModification
		propertiesModification: RwPropertiesModification new;
		yourself.
	instanceMethodsModification := classDef _methodsModificationClass
		extendedClassName: classDef name.
	classMethodsModification := classDef _methodsModificationClass
		extendedClassName: classDef name.
	isMeta
		ifTrue: [ instanceMethodsModification _addElementModification: methodModification ]
		ifFalse: [ classMethodsModification _addElementModification: methodModification ].
	classModification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.

	loadedPackage := loadedClass loadedPackage.
	packageDef := loadedPackage asDefinition.
	packageModification := RwPackageModification
		before: packageDef
		after: packageDef.
	packageModification propertiesModification: RwPropertiesModification new.
	classesModification := loadedClass isLoadedClassExtension
		ifTrue: [ RwClassesModification new ]
		ifFalse: [ RwClassExtensionsModification new ].
	classesModification _addElementModification: classModification.
	loadedClass isLoadedClassExtension
		ifTrue: [ 
			packageModification
				classesModification: RwClassesModification new;
				classExtensionsModification: classesModification ]
		ifFalse: [ 
			packageModification
				classesModification: classesModification;
				classExtensionsModification: RwClassExtensionsModification new ].

	loadedProject := loadedPackage loadedProject.
	projectDef := loadedProject asDefinition.
	projectModification := RwProjectModification
		before: projectDef
		after: projectDef.
	projectModification propertiesModification: RwPropertiesModification new.
	packagesModification := RwPackagesModification new.
	projectModification packagesModification: packagesModification.
	packagesModification _addElementModification: packageModification.

	projectSetModification := RwProjectSetModification new _addElementModification: projectModification.

	Rowan image
		applyModification_V2: projectSetModification
		instanceMigrator: Rowan platform instanceMigrator
		symbolList: symbolList
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> isExtensionMethod: methodSelector forClassNamed: className isMeta: isMeta [

	"Answer true if the specified method is a Rowan extension method"

	| loadedMethod loadedClassOrExtension loadedPackage |

	loadedMethod := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			"No loaded method found ... it is not an extension method"
			^ false ].
	loadedPackage := loadedMethod loadedPackage.

	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , loadedPackage name printString , '.' ].

	^loadedClassOrExtension isLoadedClassExtension

]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> moveClassNamed: className toPackage: packageName [
	"Move class to <packageName>, whether or not it has been packaged. The methods in the class that are in the
		original package of the class are also moved to the new package. If the class was originally unpackaged,
		then only unpackaged methods (class and instance side) are moved to the new package."

	| class |
	class := Rowan globalNamed: className.
	(self _loadedClassNamed: className ifAbsent: [  ])
		ifNil: [ 
			| instanceSelectors classSelectors classPackageName |
			"unpackaged class"
			classPackageName := Rowan unpackagedName.
			instanceSelectors := Set new.
			classSelectors := Set new.
			class
				methodsDo: [ :selector :method | 
					method rowanPackageName = classPackageName
						ifTrue: [ instanceSelectors add: selector ] ].
			class class
				methodsDo: [ :selector :method | 
					method rowanPackageName = classPackageName
						ifTrue: [ classSelectors add: selector ] ].
			Rowan packageTools adopt
				adoptClassNamed: className
				instanceSelectors: instanceSelectors
				classSelectors: classSelectors
				intoPackageNamed: packageName ]
		ifNotNil: [ :loadedClass | 
			| destinationLoadedPackage sourceLoadedPackage projectSetDefinition projectDef packageDef clsDef destProjectDef srcPackageDef srcClsDef destPackageDef |
			"packaged class"
			destinationLoadedPackage := Rowan image loadedPackageNamed: packageName.
			sourceLoadedPackage := loadedClass loadedPackage.
			destinationLoadedPackage name = sourceLoadedPackage name
				ifTrue: [ 
					"class is already in the desired package, never mind"
					^ self ].
			projectSetDefinition := RwProjectSetDefinition new.
			projectDef := sourceLoadedPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: sourceLoadedPackage name.
			clsDef := packageDef classDefinitions at: className.

			projectSetDefinition addProject: projectDef.
			destinationLoadedPackage loadedProject name = projectDef name
				ifTrue: [ destProjectDef := projectDef ]
				ifFalse: [ 
					"class is moving to a different project"
					destProjectDef := destinationLoadedPackage loadedProject asDefinition.
					projectSetDefinition addProject: destProjectDef ].

			srcPackageDef := projectDef packageNamed: sourceLoadedPackage name.
			srcClsDef := srcPackageDef removeClassNamed: className.

			destPackageDef := destProjectDef packageNamed: destinationLoadedPackage name.
			(destPackageDef classDefinitions at: className ifAbsent: [  ])
				ifNotNil: [ 
					self
						error:
							'A class definition for the class ' , className printString
								, ' already exists in the destination package '
								, packageName printString ].

			srcClsDef
				moveToPackageNamed: destinationLoadedPackage name
				packageConvention: destinationLoadedPackage loadedProject packageConvention.
			destPackageDef classDefinitions at: className put: srcClsDef.

			Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> moveMethod: methodSelector forClassNamed: className isMeta: isMeta toPackage: packageName [
	"Move the method into <packageName>, whether or not it has been packaged"

	(self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [  ])
		ifNil: [ 
			"adopt an unpackaged method into the named package"
			Rowan packageTools adopt
				adoptMethod: methodSelector
				inClassNamed: className
				isMeta: isMeta
				intoPackageNamed: packageName ]
		ifNotNil: [ :loadedMethodToBeMoved | 
			| srcLoadedMethodPackage srcLoadedClassPackage srcLoadedClassOrExtension projectDef packageDef clsDef projectSetDefinition destinationLoadedPackage methodDef beh category srcLoadedClass |
			"Move a packaged method to another package"
			beh := Rowan globalNamed: className.
			isMeta
				ifTrue: [ beh := beh class ].
			category := beh categoryOfSelector: methodSelector asSymbol.
			destinationLoadedPackage := Rowan image loadedPackageNamed: packageName.
			srcLoadedClass := Rowan image loadedClassNamed: className ifAbsent: [ ].
			srcLoadedClass
				ifNotNil: [ srcLoadedClassPackage := srcLoadedClass loadedPackage ].
			srcLoadedMethodPackage := loadedMethodToBeMoved loadedPackage.
			srcLoadedClassOrExtension := srcLoadedMethodPackage
				classOrExtensionForClassNamed: className
				ifAbsent: [ 
					self
						error:
							'Internal error -- no class or extension for ' , className printString
								, ' in package ' , srcLoadedMethodPackage name printString , '.' ].

			projectSetDefinition := RwProjectSetDefinition new.
			projectDef := srcLoadedMethodPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: srcLoadedMethodPackage name.
			clsDef := srcLoadedClassOrExtension isLoadedClass
				ifTrue: [ packageDef classDefinitions at: className ]
				ifFalse: [ packageDef classExtensions at: className ].

			projectSetDefinition addProject: projectDef.
			destinationLoadedPackage name = srcLoadedMethodPackage name
				ifTrue: [ 
					"method is not moving to a different package, only need to change the protocol for the method"
					methodDef := isMeta
						ifTrue: [ clsDef classMethodDefinitions at: methodSelector ]
						ifFalse: [ clsDef instanceMethodDefinitions at: methodSelector ].
					methodDef protocol: category ]
				ifFalse: [ 
					| destProjectDef destPackageDef destClsDef srcPackageDef srcClsDef |
					"method is moving to a different package"
					destinationLoadedPackage loadedProject name = projectDef name
						ifTrue: [ destProjectDef := projectDef ]
						ifFalse: [ 
							"method is moving to a different project"
							destProjectDef := destinationLoadedPackage loadedProject asDefinition.
							projectSetDefinition addProject: destProjectDef ].
					srcPackageDef := projectDef packageNamed: srcLoadedMethodPackage name.
					srcClsDef := srcLoadedClassOrExtension isLoadedClass
						ifTrue: [ srcPackageDef classDefinitions at: className ]
						ifFalse: [ srcPackageDef classExtensions at: className ].

					destPackageDef := destProjectDef
						packageNamed: destinationLoadedPackage name.

					destClsDef := srcLoadedClassPackage == destinationLoadedPackage
						ifTrue: [ 
							"method is not an extension method ... add the method to the class definition"
							destPackageDef classDefinitions
								at: className
								ifAbsent: [ 
									self
										error:
											'internal error - class ' , className printString
												, ' not found in expected package '
												, destinationLoadedPackage name printString ] ]
						ifFalse: [ 
							"method is to be added as an extension method"
							destPackageDef classExtensions
								at: className
								ifAbsentPut: [ RwClassExtensionDefinition newForClassNamed: className ] ].

					destinationLoadedPackage loadedProject packageConvention ~= 'Rowan'
						ifTrue: [ 
							"need to fabricate new method protocol"
							destClsDef isClassExtension
								ifTrue: [ 
									"fabricate new class extension protocol"
									category := '*' , packageName asLowercase ]
								ifFalse: [ 
									"fabricate new non-class extensions protocol"
									category := '(as yet unclassified)' ] ].

					isMeta
						ifTrue: [ 
							methodDef := srcClsDef classMethodDefinitions at: methodSelector.
							methodDef protocol: category.
							destClsDef addClassMethodDefinition: methodDef.
							srcClsDef removeClassMethod: methodSelector ]
						ifFalse: [ 
							methodDef := srcClsDef instanceMethodDefinitions at: methodSelector.
							methodDef protocol: category.
							destClsDef addInstanceMethodDefinition: methodDef.
							srcClsDef removeInstanceMethod: methodSelector ] ].

			Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> moveMethod: methodSelector forClassNamed: className isMeta: isMeta toProtocol: hybridPackageName [

	"hybridPackageName is expected to be an existing protocol in className"

	| loadedMethodToBeMoved srcLoadedMethodPackage srcLoadedClassPackage srcLoadedClassOrExtension projectDef packageDef 
		clsDef projectSetDefinition destinationLoadedPackage methodDef |

	loadedMethodToBeMoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ ].

	destinationLoadedPackage := (hybridPackageName at: 1) = $*
		ifTrue: [ 
			Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ 
			| lc |
			lc := self
				_loadedClassNamed: className
				ifAbsent: [].
			lc 
				ifNil: [
					loadedMethodToBeMoved
						ifNil: [ 
							"unpackaged method moved to unpackaged category"
							^ (Rowan image objectNamed: className) _moveMethod: methodSelector toCategory: hybridPackageName ].
				RwPerformingUnpackagedEditNotification signal: 'Attempt to move a packaged method to an unpackaged class ', className printString, '. The unpackaged method will not be tracked by Rowan'.
				"Notification resumed, so continue with move"
				"Move packaged method to unpackaged category"
				"Disown the method, then move it to proper category" 
				Rowan packageTools disown disownMethod: methodSelector inClassNamed: className isMeta: isMeta.
				^ (Rowan image objectNamed: className) _moveMethod: methodSelector toCategory: hybridPackageName ].
			"use the loaded package for the class that contains the method"
			srcLoadedClassPackage := lc loadedPackage ].

	loadedMethodToBeMoved
		ifNil: [
			| instanceSelectors classSelectors res |
			"Moving an unpackaged method to a package in an unpackage class"
			"Move the method to the proper category and then adopt the method"
			isMeta
				ifTrue: [
					instanceSelectors := {}.
					classSelectors := { methodSelector } ]
				ifFalse: [
					instanceSelectors := { methodSelector }.
					classSelectors := {} ].
			res := (Rowan image objectNamed: className) _moveMethod: methodSelector toCategory: hybridPackageName.
			Rowan packageTools adopt
				adoptClassNamed: className 
				classExtension: true
				instanceSelectors: instanceSelectors 
				classSelectors: classSelectors 
				intoPackageNamed: destinationLoadedPackage name.
			^ res ].

	"Moving a packaged method to another package"
	srcLoadedMethodPackage := loadedMethodToBeMoved loadedPackage.
	srcLoadedClassOrExtension := srcLoadedMethodPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , srcLoadedMethodPackage name printString , '.' ].

	projectSetDefinition := RwProjectSetDefinition new.
	projectDef := srcLoadedMethodPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: srcLoadedMethodPackage name.
	clsDef := srcLoadedClassOrExtension isLoadedClass
		ifTrue: [ packageDef classDefinitions at: className ]
		ifFalse: [ packageDef classExtensions at: className ].

	projectSetDefinition addProject: projectDef.
	destinationLoadedPackage name = srcLoadedMethodPackage name
		ifTrue: [
			"method is not moving to a different package, only need to change the protocol for the method"

			methodDef := isMeta
				ifTrue: [ clsDef classMethodDefinitions at: methodSelector ]
				ifFalse: [ clsDef instanceMethodDefinitions at: methodSelector ].
			methodDef protocol: hybridPackageName ]
		ifFalse: [
			"method is moving to a different package"

			| destProjectDef destPackageDef destClsDef srcPackageDef  srcClsDef |
			destinationLoadedPackage loadedProject name = projectDef name 
				ifTrue: [ destProjectDef := projectDef ]
				ifFalse: [ 
					"method is moving to a different project"
					destProjectDef := destinationLoadedPackage loadedProject asDefinition.
					projectSetDefinition addProject: destProjectDef ].
			srcPackageDef := projectDef packageNamed: srcLoadedMethodPackage name.
			srcClsDef := srcLoadedClassOrExtension isLoadedClass
				ifTrue: [ srcPackageDef classDefinitions at: className ]
				ifFalse: [ srcPackageDef classExtensions at: className ].

			destPackageDef := destProjectDef packageNamed: destinationLoadedPackage name. 

			destClsDef := srcLoadedClassPackage == destinationLoadedPackage
				ifTrue: [
					"method is not an extension method ... add the method to the class definition"
					destPackageDef 
						classDefinitions 
							at: className 
							ifAbsent: [ 
								self error: 'internal error - class ', 
									className printString , 
									' not found in expected package ', 
									destinationLoadedPackage name printString ] ]
				ifFalse: [ 
					"method is to be added as an extension method" 
					destPackageDef classExtensions at: className ifAbsentPut: [ RwClassExtensionDefinition newForClassNamed: className ] ].
			isMeta
				ifTrue: [
					methodDef := srcClsDef classMethodDefinitions at: methodSelector.
					methodDef protocol: hybridPackageName.
					destClsDef addClassMethodDefinition: methodDef.
					srcClsDef removeClassMethod: methodSelector ]
				ifFalse: [ 
					methodDef := srcClsDef instanceMethodDefinitions at: methodSelector.
					methodDef protocol: hybridPackageName.
					destClsDef addInstanceMethodDefinition: methodDef.
					srcClsDef removeInstanceMethod: methodSelector ] ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
]

{ #category : 'project browsing' }
RwPrjBrowserToolV2 >> projectNamed: projectName [

	^ self _projectNamed: projectName
]

{ #category : 'definition updating' }
RwPrjBrowserToolV2 >> projectNamed: projectName updateDefinition: updateBlock [

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	updateBlock value: projectDefinition.
	projectDefinition load
]

{ #category : 'definition updating' }
RwPrjBrowserToolV2 >> projectsNamed: projectNames updateDefinition: updateBlock [

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectNames
		do: [ :projectName | 
			| projectDefinition |
			projectDefinition := self _projectNamed: projectName.
			updateBlock value: projectDefinition.
			projectSetDefinition addProject: projectDefinition ].
	self class load loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> removeClassNamed: className [

	| projectSetDefinition loadedClass loadedClassProjectDef  loadedClassProjectDefName loadedClassExtensions |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedClass := self _loadedClassNamed: className ifAbsent: [ ].
	loadedClassExtensions := #(). "only apply if the class itself is not managed by Rowan"

	loadedClass 
		ifNil: [
			"Give developer a chance to handle and abort the removal of unpackaged class"
			RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged class ', className printString, '. The removal will not be tracked by Rowan'.
			"Notification resumed, so continue with removal ... actual removal is performed below"
				loadedClassExtensions := self _loadedClassExtensionsNamed: className ifAbsent: [ #() ] ]
		ifNotNil: [
			| loadedPackage projectDef packageDef classDef |
			loadedPackage := loadedClass loadedPackage.
			projectDef := loadedClass loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			classDef := packageDef classDefinitions at: loadedClass name.
			packageDef removeClassDefinition: classDef.
			projectSetDefinition addProject: projectDef.
			loadedClassProjectDef := projectDef.
			loadedClassProjectDefName := projectDef name ].

	loadedClassExtensions
		do: [ :loadedClassExtension | 
			| loadedClassExtensionPackage ceProjectDef cePackageDef |
			loadedClassExtensionPackage := loadedClassExtension loadedPackage.
			ceProjectDef := loadedClassExtension loadedProject asDefinition.
			ceProjectDef name = loadedClassProjectDefName
				ifTrue: [
					"extensions defined in same project as the class itself, entry already exists in projectSetDefinition"
					ceProjectDef := loadedClassProjectDef ]
				ifFalse: [ projectSetDefinition addProject: ceProjectDef ].
			cePackageDef := ceProjectDef packageNamed: loadedClassExtensionPackage name.
			cePackageDef
				removeClassExtensionDefinition:
					(cePackageDef classExtensions at: loadedClassExtension name) ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass 
		ifNil: [
			"need to do the actual removal of the unpackaged class after Rowan has done it's job"
			| theClass |
			theClass := Rowan image objectNamed: className.
			(Rowan image symbolList dictionariesAndSymbolsOf: theClass)
				do: [:ar | | dict key |
					"brute force removal of the class from system dictionaries"
					dict := ar at: 1.	
					key := ar at: 2.
					dict removeKey: key ] ]
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> removeMethod: methodSelector forClassNamed: className isMeta: isMeta [

	"Remove the given method selector from the class named className"

	| loadedMethodToBeRemoved loadedPackage loadedClassOrExtension projectDef packageDef crDef |
	loadedMethodToBeRemoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [
			| theBehavior |
			theBehavior := Rowan globalNamed: className.
			isMeta ifTrue: [ theBehavior := theBehavior class ].
			RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged method from the class ', className printString, '. The removal will not be tracked by Rowan'.
			"Notification resumed, so continue with removal"
			^ theBehavior removeSelector: methodSelector ].
	loadedPackage := loadedMethodToBeRemoved loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , loadedPackage name printString , '.' ].
	projectDef := loadedPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	crDef := loadedClassOrExtension isLoadedClass
		ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
		ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
	isMeta
		ifTrue: [ crDef removeClassMethod: methodSelector ]
		ifFalse: [ crDef removeInstanceMethod: methodSelector ].
	projectDef load
]

{ #category : 'package browsing' }
RwPrjBrowserToolV2 >> removePackageNamed: packageName [

	| loadedPackage projectDef |
	loadedPackage := self _loadedPackageNamed: packageName.
	projectDef := loadedPackage loadedProject asDefinition.
	projectDef removePackageNamed: loadedPackage name.

	projectDef load
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> removeProtocol: hybridPackageName fromClassNamed: className isMeta: isMeta [

	"hybridPackageName is expected to be an existing protocol in className, that may or may not be empty"

	| projectSetDefinition loadedClass theClass theBehavior loadedPackage |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedClass := self _loadedClassNamed: className ifAbsent: [ ].

	loadedPackage := (hybridPackageName at: 1) = $*
		ifTrue: [ 
			Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ loadedClass loadedPackage ].

	theClass := Rowan image objectNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].

	(((theBehavior selectorsIn: hybridPackageName)
		collect: [:methodSelector |
			self
				_loadedMethod: methodSelector
				inClassNamed: className
				isMeta: isMeta
				ifAbsent: [
					RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged method from the class ', className printString, '. The removal will not be tracked by Rowan'.
					"Notification resumed, so continue with removal"
					(Rowan image objectNamed: className) removeSelector: methodSelector.
					nil "no loaded method involved" ] ]) select: [:each | each notNil ])
			do: [:loadedMethodToBeRemoved |
				| loadedClassOrExtension projectDef crDef packageDef methodSelector | 

				methodSelector := loadedMethodToBeRemoved selector.
				loadedPackage == loadedMethodToBeRemoved loadedPackage
					ifFalse: [ self error: 'Internal error -- the loaded package of a method to be removed does not match the expected loaded package' ].
				loadedClassOrExtension := loadedPackage
					classOrExtensionForClassNamed: className
					ifAbsent: [ 
						self
							error:
								'Internal error -- no class or extension for ' , className printString
									, ' in package ' , loadedPackage name printString , '.' ].
				projectDef := projectSetDefinition
					projectNamed: loadedPackage loadedProject name
					ifAbsent: [ 
						projectDef := loadedPackage loadedProject asDefinition.
						projectSetDefinition addProject: projectDef.
						projectDef ].
				packageDef := projectDef packageNamed: loadedPackage name.
				crDef := loadedClassOrExtension isLoadedClass
					ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
					ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
				isMeta
					ifTrue: [ crDef removeClassMethod: methodSelector ]
					ifFalse: [ crDef removeInstanceMethod: methodSelector ] ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"Rowan does not automatically remove empty method categories ... just removes methods,
		we need to clean up after Rowan is done."
	theBehavior removeCategory: hybridPackageName asSymbol
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> renameClassNamed: className to: newName [

	"During renameClassNamed:to: a class is created with the new name and all methods 
		are copied from the old class to the new class. If there are subclasses of the old class,
		the subclasses are moved under the new class, then the old class is removed. It is
		recommended that before renaming a class, you should find references to the class 
		and be prepared to edit the methods once the rename is complete.

	Worry about the fact that any references to the original class that may be in methods in 
		the class itself or in subclass methods will error out, if recompiled 
	"

	"anser the new copy of the class"

	| projectSetDefinition loadedClass loadedPackage loadedProject projectDef packageDef |
"1. find references to the original class [not yet implemented]"

"2. copy class to renamed class"
	projectSetDefinition := self _copyClassDefinitionNamed: className to: newName.

	loadedClass := self 
		_loadedClassNamed: className 
		ifAbsent: [  self error: 'No loaded class named: ', className printString , ' found.' ].

"3. remove original class definition"
	loadedPackage := loadedClass loadedPackage.
	loadedProject := loadedPackage loadedProject.
	projectDef := projectSetDefinition projectNamed: loadedProject name ifAbsent: [ self error: 'No loaded project named: ', loadedProject printString , ' found.'].
	packageDef := projectDef packageNamed: loadedPackage name.

	packageDef removeClassNamed: className.

"4. change superclass for all subclasses of original class to renamed class"
	(Rowan globalNamed: className) subclasses do: [:subclass |
		| subclassName classDef  |
		subclassName := subclass name asString.
		loadedClass := self 
			_loadedClassNamed: subclassName
			ifAbsent: [  self error: 'No loaded class named: ', subclassName printString , ' found.' ].

		loadedPackage := loadedClass loadedPackage.
		loadedProject := loadedPackage loadedProject.
		projectDef := projectSetDefinition 
			projectNamed: loadedProject name 
			ifAbsent: [ 
				| pDef |
				pDef := loadedPackage loadedProject asDefinition.
				projectSetDefinition addProject: pDef.
				pDef ].
		packageDef := projectDef packageNamed: loadedPackage name.
		classDef := packageDef classDefinitions at: subclassName.
		classDef superclassName: newName ].

"load projectSetDefinition & do rename"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	^ Rowan globalNamed: newName
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> unpackageClass: class [
	"Unpackage the given class and all methods in the class that in the same package, 
		while leaving the class installed in the image"

	| loadedClass loadedPackage |
	loadedClass := Rowan image
		loadedClassForClass: class
		ifAbsent: [ 
			"the class is not packaged, so we are done"
			^ self ].
	loadedClass loadedInstanceMethods values
		do: [ :loadedMethod | 
			loadedClass removeLoadedMethod: loadedMethod.
			loadedMethod unpackageMethod ].
	loadedClass loadedClassMethods values
		do: [ :loadedMethod | 
			loadedClass removeLoadedMethod: loadedMethod.
			loadedMethod unpackageMethod ].

	loadedPackage := loadedClass loadedPackage.
	loadedPackage removeLoadedClass: loadedClass.
	RwGsSymbolDictionaryRegistry_ImplementationV2
		unregisterLoadedClass: loadedClass
		forClass: class
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> unpackageClassNamed: className [
	"Unpackage the given class and all methods in the class that in the same package, 
		while leaving the class installed in the image"

	| theClass |
	theClass := Rowan globalNamed: className.
	theClass
		ifNil: [ self error: 'No class named ' , className printString , ' found' ].
	self unpackageClass: theClass
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> unpackageMethod: method [
	"unpackage the given method, while leaving the method installed in the image"

	| loadedMethod loadedClassOrExtension loadedPackage packageName |
	packageName := method rowanPackageName.
	packageName = Rowan unpackagedName
		ifTrue: [ 
			"already unpackaged, nothing else to do"
			^ self ].
	loadedMethod := Rowan image loadedMethodForMethod: method.
	loadedPackage := loadedMethod loadedPackage.
	loadedClassOrExtension := loadedMethod loadedClass.
	loadedClassOrExtension isLoadedClassExtension
		ifTrue: [ 
			loadedClassOrExtension isEmpty
				ifTrue: [ 
					RwGsSymbolDictionaryRegistry_ImplementationV2
						unregisterLoadedClassExtension: loadedClassOrExtension
						forClass: loadedClassOrExtension handle.
					loadedPackage removeLoadedClassExtension: loadedClassOrExtension ] ].
	loadedMethod unpackageMethod
]

{ #category : 'method browsing' }
RwPrjBrowserToolV2 >> unpackageMethod: methodSelector forClassNamed: className isMeta: isMeta [
	"unpackage the given method, while leaving the method installed in the image"

	| theBehavior theMethod |
	theBehavior := Rowan globalNamed: className.
	isMeta
		ifTrue: [ theBehavior := theBehavior class ].
	theMethod := theBehavior compiledMethodAt: methodSelector.
	self unpackageMethod: theMethod
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> updateClassCategory: aString forClassNamed: className [
	"update the category of the named class"

	| loadedClass projectDefinition packageDefinition classDefinition |
	loadedClass := Rowan image
		loadedClassNamed: className
		ifAbsent: [ 
			RwPerformingUnpackagedEditNotification
				signal:
					'Attempt to add or modify a category for the class ' , className printString
						, '. The modification will not be tracked by Rowan'.	"Notification resumed, so continue with add/modify"
			^ (Rowan globalNamed: className) category: aString ].

	projectDefinition := loadedClass loadedProject asDefinition.
	packageDefinition := projectDefinition
		packageNamed: loadedClass loadedPackage name.
	classDefinition := packageDefinition classDefinitions at: loadedClass name.
	(Rowan image
		validClassCategory: aString
		forLoadedPackage: loadedClass loadedPackage)
		ifFalse: [ 
			self
				error:
					'Category ' , aString printString , ' for class ' , className printString
						, 'does not follow ' , loadedClass loadedProject packageConvention
						, ' package convention' ].
	classDefinition category: aString.

	projectDefinition load
]

{ #category : 'class browsing' }
RwPrjBrowserToolV2 >> updateClassComment: aString forClassNamed: className [ 

	"update the comment of the named class"

	| loadedClass projectDefinition packageDefinition classDefinition |
	loadedClass := Rowan image 
		loadedClassNamed: className 
		ifAbsent: [
			RwPerformingUnpackagedEditNotification signal: 'Attempt to add or modify a comment for the class ', className printString, '. The modification will not be tracked by Rowan'.
			"Notification resumed, so continue with add/modify"
			^ (Rowan globalNamed: className) comment: aString ].

	projectDefinition := loadedClass loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: loadedClass loadedPackage name.
	classDefinition := packageDefinition classDefinitions at: loadedClass name.
	classDefinition comment: aString.

	projectDefinition load.
]
