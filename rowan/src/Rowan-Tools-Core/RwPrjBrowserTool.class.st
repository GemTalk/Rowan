Class {
	#name : 'RwPrjBrowserTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjBrowserTool >> _copyClassDefinitionNamed: className to: newName [

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
			packageDef addClassExtension: classExtDef ].

	^ projectSetDefinition
]

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

{ #category : 'private' }
RwPrjBrowserTool >> _rowanSymbolDictionaryNames [

	^ #( #RowanKernel #RowanLoader #RowanTools )

]

{ #category : 'class browsing' }
RwPrjBrowserTool >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName constraints: constraintsArray options: optionsArray [

	| loadedPackage loadedProject loadedClass theOptions theConstraints |
	loadedPackage := self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	loadedProject := loadedPackage loadedProject.
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	loadedClass := self _loadedClassNamed: className ifAbsent: [  ].
	theOptions := optionsArray collect: [:each | each asString ].
	(constraintsArray isKindOf: Array)
		ifTrue: [
			theConstraints := {}.
			constraintsArray do: [:ar |
				(ar isKindOf: Array)
					ifTrue: [ theConstraints add: { (ar at: 1) asString . (ar at: 2) name asString } ]
					ifFalse: [ theConstraints add: ar name asString ] ] ]
		ifFalse: [
			theConstraints := constraintsArray
				ifNil: [ #() ]
				ifNotNil: [ constraintsArray ]].
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
						instVarNames: anArrayOfStrings;
						classVarNames: anArrayOfClassVars;
						classInstVarNames: anArrayOfClassInstVars;
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
RwPrjBrowserTool >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName options: optionsArray [

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
			Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ 
			| loadedClass |
			loadedClass := Rowan image 
				loadedClassNamed: className 
				ifAbsent: [
					RwPerformingUnpackagedEditNotification signal: 'Attempt to add or modify an unpackage method in the class ', className printString, '. The modification will not be tracked by Rowan'.
					"Notification resumed, so continue with add/modify"
					^ (Rowan image objectNamed: className)
						compileMethod: methodSource
						dictionaries: Rowan image symbolList
						category: hybridPackageName].
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
						ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
					"remove the method from one package and add it to the other"
					isMeta
						ifTrue: [ 
							crDef removeClassMethod: methodDef selector.
							cDef addClassMethodDefinition: methodDef  ]
						ifFalse: [ 
							crDef removeInstanceMethod: methodDef selector.
							cDef addInstanceMethodDefinition: methodDef  ] ] ].
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
	loadedPackage := Rowan image
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

{ #category : 'project browsing' }
RwPrjBrowserTool >> addRowanSymbolDictionariesToPersistentSymbolList [

	self addRowanSymbolDictionariesToPersistentSymbolListFor: System myUserProfile

]

{ #category : 'project browsing' }
RwPrjBrowserTool >> addRowanSymbolDictionariesToPersistentSymbolListFor: userProfile [

	| systemUser |
	systemUser := AllUsers userWithId: 'SystemUser'.
	self _rowanSymbolDictionaryNames do: [:symDictName |
		| aSymbolDictionary anIndex |
		aSymbolDictionary := systemUser objectNamed: symDictName.
		anIndex := userProfile symbolList size + 1.
		userProfile
			insertDictionary: aSymbolDictionary at: anIndex ]

]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForClass: aClass [

	^ self classCreationTemplateForClass: aClass hybridBrowser: false
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser [

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

	| result lfsp superClass |
	result := String new.
	superClass := Rowan image objectNamed: superclassName.
	result addAll: superclassName.
	lfsp := Character lf asString tab.
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
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName className: className type: classType instanceVariablesString: instanceVariablesString classVariablesString: classVariablesString classInstanceVariablesString: classInstanceVariablesString poolDictionariesString: poolDictionariesString 
comment: comment category: category [

	"classType may be 'normal', 'bytes', or 'variable'"

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
	classType = 'bytes'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className.
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
RwPrjBrowserTool >> copyClassNamed: className to: newName [

	"anser the new copy of the class"

	| projectSetDefinition |
	projectSetDefinition := self _copyClassDefinitionNamed: className to: newName.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	^ Rowan globalNamed: newName
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
		newForGitBasedProjectNamed: projectName.
	updateBlock cull: projectDefinition.
	self class load loadProjectDefinition: projectDefinition

]

{ #category : 'project browsing' }
RwPrjBrowserTool >> exposeRowanToolsSymbolDictionary [

	| transient size dict |
	transient := Rowan image symbolList.
	size := transient size.
	dict := (AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools'.
	transient insertObject: dict at: size + 1.

]

{ #category : 'method browsing' }
RwPrjBrowserTool >> isExtensionMethod: methodSelector forClassNamed: className isMeta: isMeta [

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

{ #category : 'method browsing' }
RwPrjBrowserTool >> moveMethod: methodSelector forClassNamed: className isMeta: isMeta toProtocol: hybridPackageName [

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
							^ (Rowan image objectNamed: className) moveMethod: methodSelector toCategory: hybridPackageName ].
				RwPerformingUnpackagedEditNotification signal: 'Attempt to move a packaged method to an unpackaged class ', className printString, '. The unpackaged method will not be tracked by Rowan'.
				"Notification resumed, so continue with move"
				"Move packaged method to unpackaged category"
				"Disown the method, then move it to proper category" 
				Rowan packageTools disown disownMethod: methodSelector inClassNamed: className isMeta: isMeta.
				^ (Rowan image objectNamed: className) moveMethod: methodSelector toCategory: hybridPackageName ].
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
			res := (Rowan image objectNamed: className) moveMethod: methodSelector toCategory: hybridPackageName.
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
				removeClassExtension:
					(cePackageDef classExtensions at: loadedClassExtension name) ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass 
		ifNil: [
			"need to do the actual removal of the unpackaged class after Rowan has done it's job"
			| theClass |
			theClass := Rowan image objectNamed: className.
			(GsCurrentSession currentSession symbolList dictionariesAndSymbolsOf: theClass)
				do: [:ar | | dict key |
					"brute force removal of the class from system dictionaries"
					dict := ar at: 1.	
					key := ar at: 2.
					dict removeKey: key ] ]
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> removeMethod: methodSelector forClassNamed: className isMeta: isMeta [

	"Remove the given method selector from the class named className"

	| loadedMethodToBeRemoved loadedPackage loadedClassOrExtension projectDef packageDef crDef |
	loadedMethodToBeRemoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [
			RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged method from the class ', className printString, '. The removal will not be tracked by Rowan'.
			"Notification resumed, so continue with removal"
			^ (Rowan globalNamed: className) removeSelector: methodSelector ].
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
	projectDef removePackageNamed: loadedPackage name.

	Rowan projectTools load loadProjectDefinition: projectDef

]

{ #category : 'class browsing' }
RwPrjBrowserTool >> removeProtocol: hybridPackageName fromClassNamed: className isMeta: isMeta [

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
RwPrjBrowserTool >> renameClassNamed: className to: newName [

	"
		1. find references to the original class
		2. copy class to renamed class
		3. remove original class
		4. change superclass for all subclasses of original class to renamed class

		Worry about the fact that the references to the original class may be in methods will error out, if recompiled 
	"

	"anser the new copy of the class"

	| projectSetDefinition loadedClass loadedPackage loadedProject projectDef packageDef |
"1. find references to the original class"

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
		classDef superclassName: className ].

"load projectSetDefinition & do rename"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	^ Rowan globalNamed: newName
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> updateClassComment: aString forClassNamed: className [ 

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

	self class load loadProjectDefinition: projectDefinition.
]
