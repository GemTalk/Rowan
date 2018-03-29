Class {
	#name : 'RwPrjBrowserTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassExtensionsNamed: className [

	^ self
		_loadedClassExtensionsNamed: className
		ifAbsent: [ 
			self
				error:
					'No extensions for the class ' , className printString , ' were found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassExtensionsNamed: className ifAbsent: absentBlock [

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | ^ loadedClassExtensionSet ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassNamed: className [

	^ self
		_loadedClassNamed: className
		ifAbsent: [ self error: 'The class ' , className printString , ' was not found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassNamed: className ifAbsent: absentBlock [

	^ Rowan image loadedClassNamed: className ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta [

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
RwPrjBrowserTool >> _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta ifAbsent: absentBlock [

	^ Rowan image
		loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | ^ loadedMethod ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedPackageNamed: packageName [

	^ self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedPackageNamed: packageName ifAbsent: absentBlock [

	^ Rowan image loadedPackageNamed: packageName ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectForClassNamed: className [

	^ (self _loadedClassNamed: className) loadedProject
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectForMethod: methodSelector inClassNamed: className isMeta: isMeta [

	^ (self _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta)
		loadedProject
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectForPackageNamed: packageName [

	^ (self _loadedPackageNamed: packageName) loadedProject
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectNamed: projectName [

	^ Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self error: 'No project named ' , projectName printString , ' found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectForClassNamed: className [

	^ (self _loadedProjectForClassNamed: className) asDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectForMethod: methodSelector inClassNamed: className isMeta: isMeta [

	^ (self
		_loadedProjectForMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta) asDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectForPackageNamed: packageName [

	^ (self _loadedProjectForPackageNamed: packageName) asDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectNamed: projectName [

	^ (self _loadedProjectNamed: projectName) asDefinition
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName options: optionsArray [

	| loadedPackage loadedProject loadedClass |
	loadedPackage := self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	loadedProject := loadedPackage loadedProject.
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	loadedClass := self _loadedClassNamed: className ifAbsent: [  ].
	loadedClass
		ifNil: [ 
			| classDef |
			"create a new class"
			classDef := RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: anArrayOfStrings
				classinstvars: anArrayOfClassInstVars
				classvars: anArrayOfClassVars
				category: category
				comment: nil
				pools: #()
				type: type.
			classDef gs_options: optionsArray.
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
						instVarNames: anArrayOfStrings;
						classVarNames: anArrayOfClassVars;
						classInstVarNames: anArrayOfClassInstVars;
						gs_options: optionsArray;
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
RwPrjBrowserTool >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: symbolDictionary category: category packageName: packageName options: optionsArray [

	| loadedPackage loadedProject loadedClass |
	loadedPackage := self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	loadedProject := loadedPackage loadedProject.
	symbolDictionary name
		=
			((loadedProject specification platformSpec at: 'gemstone')
				symbolDictNameForPackageNamed: packageName)
		ifFalse: [ 
			self
				error:
					'The symbol dictionary specified for the new class does not match the symbol dictionary specified by the Rowan project' ].
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	loadedClass := self _loadedClassNamed: className ifAbsent: [  ].
	loadedClass
		ifNil: [ 
			| classDef |
			"create a new class"
			classDef := RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: anArrayOfStrings
				classinstvars: anArrayOfClassInstVars
				classvars: anArrayOfClassVars
				category: category
				comment: nil
				pools: #()
				type: type.
			classDef gs_options: optionsArray.
			self createClass: classDef inPackageNamed: packageName ]
		ifNotNil: [ 
			self class edit
				definitionsForClassNamed: className
				ifFound: [ :classDef :packageDef :projectDef | 
					| classProjectDef projects projectSetDef |
					classProjectDef := projectDef.
					projectSetDef := RwProjectSetDefinition new.
					projectSetDef addProject: classProjectDef.
					classDef
						superclassName: superclassName;
						instVarNames: anArrayOfStrings;
						classVarNames: anArrayOfClassVars;
						classInstVarNames: anArrayOfClassInstVars;
						gs_options: optionsArray;
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
							thePackageDef addClassDefinition: classDef.
							self class load loadProjectSetDefinition: projectSetDef ] ]
				ifAbsent: [ 
					"shouldn't happen ... loadedClass known to exist"
					self error: 'unexpected missing class ' , className printString ] ].
	^ symbolDictionary at: className asSymbol
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> addOrUpdateMethod: methodSource inProtocol: hybridPackageName forClassNamed: className isMeta: isMeta [

	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	"a hybrid package name has a leading '*' followed by the name of a package ... 
 		where the hybrid package name is not expected to preserve case"

	| loadedPackage |
	loadedPackage := (hybridPackageName at: 1) = $*
		ifTrue: [ 
			Rowan
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ 
			| loadedClass |
			loadedClass := Rowan image loadedClassNamed: className.
			loadedClass loadedPackage ].

	^ self
		addOrUpdateMethod: methodSource
		inProtocol: hybridPackageName
		forClassNamed: className
		isMeta: isMeta
		inPackageNamed: loadedPackage name
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> addOrUpdateMethod: methodSource inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [

	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	| projectTools loadedPackage classExtensionDef methodDef updateBlock projectDefinition packageDefinition projectSetDefinition loadedMethodToBeRemoved |
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
		ifNotNil: [ :loadedMethod | 
			| loadedPackage |
			loadedPackage := loadedMethod loadedPackage.
			loadedPackage name = packageName
				ifTrue: [ 
					"loaded method being updated in same package, so no need to remove method definition from the package"
					 ]
				ifFalse: [ 
					| loadedClass projectDef packageDef crDef |
					"loaded method in different package than new version of method"
					projectDef := loadedPackage loadedProject asDefinition.
					projectDef name = pDef name
						ifTrue: [ 
							"both packages are in same project"
							projectDef := pDef ]
						ifFalse: [ 
							"each package in a different project, will need to load loaded method project as well"
							projectSetDefinition addProject: projectDef ].
					packageDef := projectDef packageNamed: loadedPackage name.
					loadedClass := loadedMethod loadedClass.
					crDef := loadedClass isLoadedClass
						ifTrue: [ packageDef classDefinitions at: loadedClass name ]
						ifFalse: [ packageDef classExtensions at: loadedClass name ].
					isMeta
						ifTrue: [ crDef removeClassMethod: methodDef selector ]
						ifFalse: [ crDef removeInstanceMethod: methodDef selector ] ] ].
	isMeta
		ifTrue: [ cDef updateClassMethodDefinition: methodDef ]
		ifFalse: [ cDef updateInstanceMethodDefinition: methodDef ].
	projectSetDefinition addProject: pDef.
	projectTools load loadProjectSetDefinition: projectSetDefinition.
	(self _loadedMethod: methodDef selector inClassNamed: className isMeta: isMeta)
		handle ].

	self
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName
				ifTrue: [ ^ updateBlock value: classDef value: projectDef ]
				ifFalse: [ 
					"the named package is different from the class definition package"
					 ] ]
		ifAbsent: [ 
			"no loaded class definition, so we probably need to add a class extension"
			 ].
	loadedPackage := Rowan
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'A package named ' , packageName printString , ' was not found.' ].
	projectDefinition := loadedPackage loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: packageName.

	classExtensionDef := packageDefinition classExtensions
		at: className
		ifAbsent: [ 
			"no existing class extension definition ... create a new one"
			classExtensionDef := RwClassExtensionDefinition newForClassNamed: className.

			packageDefinition addClassExtension: classExtensionDef.
			classExtensionDef ].

	^ updateBlock value: classExtensionDef value: projectDefinition
]

{ #category : 'package browsing' }
RwPrjBrowserTool >> addPackageNamed: packageName toProjectNamed: projectName [

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	projectDefinition addPackage: (RwPackageDefinition newNamed: packageName).
	Rowan projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'package browsing' }
RwPrjBrowserTool >> addPackagesNamed: packageNames toProjectNamed: projectName [

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	packageNames
		do: [ :packageName | projectDefinition addPackage: (RwPackageDefinition newNamed: packageName) ].
	Rowan projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForClass: aClass [

	^ self classCreationTemplateForClass: aClass hybridBrowser: false
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser [

	| result anArray lfsp newByteSubclass firstElement poolDicts civs superClass className thePackageName nonRowanClass |
	result := String new.
	superClass := aClass superclass.
	className := aClass name asString.
	result addAll: superClass name asString.
	lfsp := Character lf asString
		addAll: String tab;
		yourself.
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
			aClass _portableIsIndexable
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
	result
		addAll: lfsp;
		addAll: 'category: '.
	result addAll: aClass category printString.
	nonRowanClass
		ifTrue: [ 
			"if the class is unpackaged, then we need to provide for the specification of symbol dictionary into which the class will be installed"
			result
				addAll: lfsp;
				addAll: 'inDictionary: '.
			anArray := System myUserProfile dictionaryAndSymbolOf: aClass.
			(anArray == nil
				or: [ (anArray at: 1) == GsPackagePolicy current homeSymbolDict ])
				ifTrue: [ result addAll: '''''' ]
				ifFalse: [ 
					(anArray := System myUserProfile dictionaryAndSymbolOf: (anArray at: 1))
						ifNil: [ result addAll: '''''' ]
						ifNotNil: [ result addAll: (anArray at: 2) asString ] ] ]
		ifFalse: [ 
			(hybridBrowser and: [ thePackageName = aClass category ])
				ifFalse: [ 
					result
						addAll: lfsp;
						addAll: 'packageName: '.
					result addAll: thePackageName printString ] ].
	result
		add: lfsp;
		add: aClass _optionsForDefinition.
	result add: Character lf.
	^ result
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName category: category [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		category: category
		packageName: nil
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: 'NameOfSubclass'
		category: category
		packageName: packageName
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName className: className category: category [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: className
		category: category
		packageName: nil
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName className: className category: category packageName: packageName [

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: className
		type: 'normal'
		category: category
		packageName: packageName
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName className: className type: classType category: category packageName: packageNameOrNil [

	"classType may be 'normal', 'bytes', or 'variable'"

	| result anArray lfsp firstElement poolDicts civs superClass |
	result := String new.
	superClass := Rowan image objectNamed: superclassName.
	result addAll: superclassName.
	lfsp := Character lf asString
		addAll: String tab;
		yourself.
	classType = 'normal'
		ifTrue: [ result addAll: ' rwSubclass: ' ].
	classType = 'variable'
		ifTrue: [ result addAll: ' rwIndexableSubclass: ' ].
	classType = 'bytes'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className printString.
	classType ~= 'bytes'
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
	false
		ifTrue: [ 
			"don't want inDictionary: as part of the class creation, because the project 
				specifies the class to symbol dictionary mapping, but I don't want to 
				lose the logic quite yet"
			result
				addAll: lfsp;
				addAll: 'inDictionary: UserGlobals' ].
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
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName className: className type: classType instanceVariablesString: instanceVariablesString classVariablesString: classVariablesString classInstanceVariablesString: classInstanceVariablesString poolDictionariesString: poolDictionariesString comment: comment category: category [

	"classType may be 'normal', 'bytes', or 'variable'"

	| result anArray lfsp firstElement poolDicts civs |
	result := String new.
	result addAll: superclassName.
	lfsp := Character lf asString
		addAll: String tab;
		yourself.
	classType = 'normal'
		ifTrue: [ result addAll: ' rwSubclass: ' ].
	classType = 'variable'
		ifTrue: [ result addAll: ' rwIndexableSubclass: ' ].
	classType = 'bytes'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className printString.
	classType ~= 'bytes'
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
		addAll: category printString.
	result
		add: lfsp;
		add: 'options: #()';
		add: Character lf.
	^ result
]

{ #category : 'definition updating' }
RwPrjBrowserTool >> classNamed: className updateDefinition: updateBlock [

	^ self
		classNamed: className
		updateDefinition: updateBlock
		ifAbsent: [ 
			self
				error:
					'expected class definition for class ' , className printString , ' not found.' ]
]

{ #category : 'definition updating' }
RwPrjBrowserTool >> classNamed: className updateDefinition: updateBlock ifAbsent: absentBlock [

	| projectDefinition classDefinition |
	self class edit
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			projectDefinition := projectDef.
			classDefinition := classDef ]
		ifAbsent: [ ^ absentBlock value ].

	updateBlock value: classDefinition.
	self class load loadProjectDefinition: projectDefinition
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> createClass: classDefinition inPackageNamed: packageName [

	| loadedPackage projectDefinition projectTools |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package named ' , packageName printString , ' was not found' ].
	projectDefinition := loadedPackage loadedProject asDefinition.

	projectTools := Rowan projectTools.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'project browsing' }
RwPrjBrowserTool >> createGitPackageProjectNamed: projectName [

	self createGitPackageProjectNamed: projectName updateDefinition: [  ]
]

{ #category : 'definition updating' }
RwPrjBrowserTool >> createGitPackageProjectNamed: projectName updateDefinition: updateBlock [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitPackageProjectNamed: projectName.
	updateBlock cull: projectDefinition.
	self class load loadProjectDefinition: projectDefinition
]

{ #category : 'project browsing' }
RwPrjBrowserTool >> projectNamed: projectName [

	^ self _projectNamed: projectName
]

{ #category : 'definition updating' }
RwPrjBrowserTool >> projectNamed: projectName updateDefinition: updateBlock [

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	updateBlock value: projectDefinition.
	self class load loadProjectDefinition: projectDefinition
]

{ #category : 'definition updating' }
RwPrjBrowserTool >> projectsNamed: projectNames updateDefinition: updateBlock [

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
RwPrjBrowserTool >> removeClassNamed: className [

	| projectSetDefinition loadedClass loadedPackage projectDef packageDef classDef |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedClass := self _loadedClassNamed: className.
	loadedPackage := loadedClass loadedPackage.
	projectDef := loadedClass loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	classDef := packageDef classDefinitions at: loadedClass name.
	packageDef removeClassDefinition: classDef.
	projectSetDefinition addProject: projectDef.

	(self _loadedClassExtensionsNamed: className ifAbsent: [ #() ])
		do: [ :loadedClassExtension | 
			| loadedClassExtensionPackage ceProjectDef cePackageDef |
			loadedClassExtensionPackage := loadedClassExtension loadedPackage.
			ceProjectDef := loadedClassExtension loadedProject asDefinition.
			ceProjectDef name = projectDef name
				ifTrue: [ ceProjectDef := projectDef ]
				ifFalse: [ projectSetDefinition addProject: ceProjectDef ].
			cePackageDef := ceProjectDef packageNamed: loadedClassExtensionPackage name.
			cePackageDef
				removeClassExtension:
					(cePackageDef classExtensions at: loadedClassExtension name) ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> removeMethod: methodSelector forClassNamed: className isMeta: isMeta [

	"Remove the given method selector from the class named className"

	| loadedMethodToBeRemoved loadedPackage loadedClassOrExtension projectDef packageDef crDef |
	loadedMethodToBeRemoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta.
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
	Rowan projectTools load loadProjectDefinition: projectDef
]

{ #category : 'package browsing' }
RwPrjBrowserTool >> removePackageNamed: packageName [

	| loadedPackage projectDef |
	loadedPackage := self _loadedPackageNamed: packageName.
	projectDef := loadedPackage loadedProject asDefinition.
	projectDef packages removeKey: loadedPackage name.

	Rowan projectTools load loadProjectDefinition: projectDef
]
