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
			packageDef addClassExtensionDefinition: classExtDef ].

	^ projectSetDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassExtensionsNamed: className ifAbsent: absentBlock [

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | ^ loadedClassExtensionSet ]
		ifAbsent: absentBlock
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
RwPrjBrowserTool >> _loadedProjectNamed: projectName [

	^ Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self error: 'No project named ' , projectName printString , ' found' ]
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
RwPrjBrowserTool >> addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName options: optionsArray [
	| toLoadedProject theOptions afterClass beforeClass toLoadedPackage fromLoadedPackage fromLoadedProject projectModification packagesModification projectSetModification aSymbolList loadedProjects |
	afterClass := RwClassDefinition
		newForClassNamed: className
		super: superclassName
		instvars: (anArrayOfStrings collect: [ :each | each asString ])
		classinstvars: (anArrayOfClassInstVars collect: [ :each | each asString ])
		classvars: (anArrayOfClassVars collect: [ :each | each asString ])
		category: category
		comment: nil
		pools: #()
		type: type.
	afterClass gs_options: theOptions.
	toLoadedPackage := self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	toLoadedProject := toLoadedPackage loadedProject.
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	(self _loadedClassNamed: className ifAbsent: [  ])
		ifNil: [ fromLoadedProject := toLoadedProject ]
		ifNotNil: [ :loadedClass | 
			beforeClass := loadedClass asDefinition.
			beforeClass gs_symbolDictionary: loadedClass gs_symbolDictionary.
			fromLoadedPackage := loadedClass loadedPackage.
			fromLoadedProject := loadedClass loadedProject.
			fromLoadedProject name = toLoadedProject name
				ifFalse: [ self error: 'Not handling method moves between projects, yet' ] ].

	fromLoadedProject
		ifNotNil: [ 
			fromLoadedProject name = toLoadedPackage loadedProject name
				ifFalse: [ self error: 'Not handling method moves between projects, yet' ] ].

	theOptions := optionsArray collect: [ :each | each asString ].
	(Rowan image validClassCategory: category forLoadedPackage: toLoadedPackage)
		ifFalse: [ 
			self
				error:
					'Category ' , category printString , ' for class ' , className printString
						, 'does not follow ' , toLoadedProject packageConvention
						, ' package convention' ].
	projectModification := RwProjectModification
		before: fromLoadedProject
		after: toLoadedProject.
	fromLoadedPackage
		ifNil: [ 
			"add a new class definition"
			packagesModification := RwPackagesModification new.
			self
				addPackagesModificationsForClassDefinition: afterClass
				beforeClass: beforeClass
				fromPackage: toLoadedPackage
				toPackage: toLoadedPackage
				using: packagesModification ]
		ifNotNil: [ 
			fromLoadedPackage name = packageName
				ifTrue: [ 
					"update existing class definition"
					packagesModification := RwPackagesModification new.
					self
						addPackagesModificationsForClassDefinition: afterClass
						beforeClass: beforeClass
						fromPackage: fromLoadedPackage
						toPackage: toLoadedPackage
						using: packagesModification ]
				ifFalse: [ ^ self moveClassDefinition: afterClass toPackage: packageName ] ].
	projectModification packagesModification: packagesModification.
	projectSetModification := RwProjectSetModification new.
	projectModification isEmpty
		ifFalse: [ projectSetModification addElementModification: projectModification ].
	projectSetModification
		updateForPackageMoves;
		updateForClassMoves;
		updateForMethodMoves.
	loadedProjects := Rowan projectTools load
		loadProjectSetModification: projectSetModification
		symbolList: aSymbolList.
	^ (self _loadedClassNamed: className ifAbsent: [  ]) handle currentVersion
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> addOrUpdateMethod: methodSource dictionaries: aSymbolList inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	| beforeMethod afterMethod classDefinition classModification fromLoadedMethod packageModification classesModification projectModification packagesModification loadedProjects fromLoadedPackage fromLoadedProject toLoadedPackage toLoadedProject theLoadedClass projectSetModification methodModification methodsModification |
	afterMethod := RwMethodDefinition newForSource: methodSource protocol: protocol.
	toLoadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'A package named ' , packageName printString , ' was not found.' ].
	toLoadedProject := toLoadedPackage loadedProject.
	(self
		_loadedMethod: afterMethod selector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [  ])
		ifNil: [ 
			"adding method to class ... need beforeMethod and classDefinition ... fromLoadedPackage isNil --- to indicate new method"
			beforeMethod := RwMethodDefinition new.
			theLoadedClass := Rowan image loadedClassNamed: className ]
		ifNotNil: [ :loadedMethod | 
			"updating method in class ..."
			fromLoadedMethod := loadedMethod.
			fromLoadedPackage := fromLoadedMethod loadedPackage.
			theLoadedClass := loadedMethod loadedClass.
			fromLoadedProject := fromLoadedPackage loadedProject.
			beforeMethod := fromLoadedMethod asDefinition	"derive definitions ... need beforeMethod and classDefinition" ].

	fromLoadedProject
		ifNil: [ fromLoadedProject := toLoadedProject ]
		ifNotNil: [ 
			fromLoadedProject name = toLoadedPackage loadedProject name
				ifFalse: [ self error: 'Not handling method moves between projects, yet' ] ].	"

	ENTERING THE FABRICATION ZONE
		1. _projectDefinitionPlatformConditionalAttributes is not being handled
		2. gs_symbolDictionary: for packages and class defs is not being done either"

	projectModification := RwProjectModification
		before: fromLoadedProject
		after: toLoadedProject.
	packagesModification := RwPackagesModification new.
	fromLoadedPackage isNil
		ifTrue: [ 
			"add method"
			classDefinition := theLoadedClass packageName = packageName
				ifTrue: [ 
					"adding method to package of class"
					classesModification := RwClassesModification new.
					theLoadedClass asDefinition ]
				ifFalse: [ 
					"adding method to extension package"
					classesModification := RwClassExtensionsModification new.
					(toLoadedPackage
						classOrExtensionForClassNamed: className
						ifAbsent: [ RwClassExtensionDefinition withProperties: theLoadedClass properties copy ])
						asDefinition ].
			methodModification := theLoadedClass loadedPackage name = packageName
				ifTrue: [ afterMethod compareAgainstBase: beforeMethod ]
				ifFalse: [ afterMethod compareExtensionMethodsAgainstBase: beforeMethod ].
			methodModification
				isMeta: isMeta;
				classDefinition: classDefinition;
				yourself.
			methodsModification := RwMethodsModification extendedClassName: className.
			methodModification isEmpty
				ifFalse: [ methodsModification addElementModification: methodModification ].
			classModification := classDefinition compareAgainstBase: classDefinition.
			methodsModification isEmpty
				ifFalse: [ 
					isMeta
						ifTrue: [ classModification classMethodsModification: methodsModification ]
						ifFalse: [ classModification instanceMethodsModification: methodsModification ] ].
			classModification isEmpty
				ifFalse: [ classesModification addElementModification: classModification ].
			packageModification := toLoadedPackage compareAgainstBase: toLoadedPackage.
			classesModification isEmpty
				ifFalse: [ packageModification classesModification: classesModification ].
			packageModification isEmpty
				ifFalse: [ packagesModification addElementModification: packageModification ] ]
		ifFalse: [ 
			"update/move existing method"
			fromLoadedPackage name ~= toLoadedPackage name
				ifTrue: [ 
					"method package move - remove before from fromPackage and add after to toPackage"
					packagesModification := self
						packagesModificationForRemoveMethod: fromLoadedMethod
						fromClass: theLoadedClass
						isMeta: isMeta
						fromPackage: fromLoadedPackage ].	"
			add after to toPackage"
			self
				addPackagesModificationsForMethodDefinition: afterMethod
				toClass: theLoadedClass
				isMeta: isMeta
				toPackage: toLoadedPackage
				using: packagesModification ].
	projectModification packagesModification: packagesModification.
	projectSetModification := RwProjectSetModification new.
	projectModification isEmpty
		ifFalse: [ projectSetModification addElementModification: projectModification ].
false ifTrue: [ 
	projectSetModification
		updateForPackageMoves;
		updateForClassMoves;
		updateForMethodMoves.
].
	loadedProjects := Rowan projectTools load
		loadProjectSetModification: projectSetModification
		symbolList: aSymbolList.
	^ (self
		_loadedMethod: afterMethod selector
		inClassNamed: className
		isMeta: isMeta) handle
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> addOrUpdateMethod: methodSource inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [
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

{ #category : 'package browsing' }
RwPrjBrowserTool >> addPackageNamed: packageName toComponentNamed: componentName andProjectNamed: projectName [
	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	projectDefinition addPackageNamed: packageName toComponentNamed: componentName.
	projectDefinition load
]

{ #category : 'package browsing' }
RwPrjBrowserTool >> addPackageNamed: packageName toProjectNamed: projectName [

	^ self addPackageNamed: packageName toComponentNamed: 'Core' andProjectNamed: projectName
]

{ #category : 'modification' }
RwPrjBrowserTool >> addPackagesModificationsForClassDefinition: theClassDefinition beforeClass: beforeClassOrNil fromPackage: fromLoadedPackage toPackage: toLoadedPackage using: packagesModification [
	"loadProjectSetModification:symbolList: call is expected to be done by caller"
	| beforeClassDefinition afterClassDefinition packageName className classModification classesModification  packageModification fromLoadedProject toPackageDefinition toLoadedProject fromPackageDefinition |

	packageName := toLoadedPackage name.
	className := theClassDefinition name.
	fromLoadedProject := fromLoadedPackage loadedProject asDefinition.
	fromLoadedProject name = toLoadedPackage loadedProject name
		ifFalse: [ self error: 'Not handling method moves between projects, yet' ].
	fromPackageDefinition := fromLoadedPackage asDefinition.
	beforeClassOrNil 
		ifNil: [ 
			beforeClassDefinition := RwClassDefinition new.
			afterClassDefinition := theClassDefinition]
		ifNotNil: [
			beforeClassDefinition := beforeClassOrNil.
			afterClassDefinition := theClassDefinition copy.
			afterClassDefinition gs_symbolDictionary: beforeClassDefinition gs_symbolDictionary ].

	toPackageDefinition := toLoadedPackage asDefinition.
	toLoadedProject := toLoadedPackage loadedProject.

	classModification := afterClassDefinition compareAgainstBase: beforeClassDefinition.
	classesModification := RwClassesModification new.
	classModification isEmpty
		ifFalse: [ classesModification addElementModification: classModification ].
	packageModification := toPackageDefinition
		compareAgainstBase: fromPackageDefinition.
	classesModification isEmpty
		ifFalse: [ packageModification classesModification: classesModification ].
	packageModification isEmpty
		ifFalse: [ packagesModification addElementModification: packageModification ].

]

{ #category : 'modification' }
RwPrjBrowserTool >> addPackagesModificationsForMethodDefinition: afterMethod toClass: theLoadedClass isMeta: isMeta toPackage: toPackageDefinition using: packagesModification [
	| beforeMethod packageName methodModification methodsModification classDefinition classOrExtensionDefinition className classModification classesModification fromPackageDefinition packageModification |
	beforeMethod := RwMethodDefinition new.
	packageName := toPackageDefinition name.
	classDefinition := theLoadedClass asDefinition.
	classOrExtensionDefinition := theLoadedClass packageName = packageName 
		ifTrue: [ classDefinition ]
		ifFalse: [ 
			| templateClassDefinition |
			templateClassDefinition := classDefinition copy.
			RwClassExtensionDefinition
				withProperties: templateClassDefinition properties
				classMethodDefinitions: templateClassDefinition classMethodDefinitions
				instanceMethodDefinitions: templateClassDefinition instanceMethodDefinitions ].
	className := classDefinition name.
	fromPackageDefinition := toPackageDefinition.

	methodModification := theLoadedClass loadedPackage name = packageName
		ifTrue: [ afterMethod compareAgainstBase: beforeMethod ]
		ifFalse: [ afterMethod compareExtensionMethodsAgainstBase: beforeMethod ].

	methodModification
		isMeta: isMeta;
		classDefinition: classDefinition;
		yourself.
	methodsModification := RwMethodsModification extendedClassName: className.
	methodModification isEmpty
		ifFalse: [ methodsModification addElementModification: methodModification ].
	classModification := classOrExtensionDefinition compareAgainstBase: classDefinition.
	methodsModification isEmpty
		ifFalse: [ 
			isMeta
				ifTrue: [ classModification classMethodsModification: methodsModification ]
				ifFalse: [ classModification instanceMethodsModification: methodsModification ] ].
	classesModification := RwClassesModification new.
	classModification isEmpty
		ifFalse: [ classesModification addElementModification: classModification ].
	packageModification := toPackageDefinition
		compareAgainstBase: fromPackageDefinition.
	classesModification isEmpty
		ifFalse: [ packageModification classesModification: classesModification ].
	packageModification isEmpty
		ifFalse: [ packagesModification addElementModification: packageModification ]
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
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName className: className type: classType instanceVariablesString: instanceVariablesString classVariablesString: classVariablesString classInstanceVariablesString: classInstanceVariablesString poolDictionariesString: poolDictionariesString 
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
	projectDefinition load
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

{ #category : 'definition updating' }
RwPrjBrowserTool >> moveClassDefinition: classDefinition toPackage: packageName [
	"Move <classDefinition> to <packageName>,. The methods in the class that are in the original package of the class are also moved to the new package."

	| className fromLoadedPackage toLoadedPackage projectSetModification beforeClassDefinition afterClassDefinition loadedClass packagesModification classModification classesModification packageModification projectModification fromLoadedProject toLoadedProject loadedProjects |
	className := classDefinition name.
	(loadedClass := self _loadedClassNamed: className ifAbsent: [  ])
		ifNil: [ 
			self
				error:
					'Cannot move a class definition (' , className , ') that is not loaded' ].

	toLoadedPackage := Rowan image loadedPackageNamed: packageName.
	fromLoadedPackage := loadedClass loadedPackage.
	toLoadedPackage name = fromLoadedPackage name
		ifTrue: [ 
			"class is already in the desired package, never mind"
			^ self ].
	fromLoadedProject := fromLoadedPackage loadedProject.
	toLoadedProject := toLoadedPackage loadedProject.
	beforeClassDefinition := loadedClass asDefinition.
	afterClassDefinition := beforeClassDefinition copy.
	afterClassDefinition
		moveToPackageNamed: packageName
		packageConvention: toLoadedProject packageConvention.	"take care of category renaming that might be necessary"

	projectSetModification := RwProjectSetModification new.
	packagesModification := RwPackagesModification new.

	classModification := afterClassDefinition
		compareAgainstBase: beforeClassDefinition.
	classesModification := RwClassesModification new.
	classModification isEmpty
		ifFalse: [ classesModification addElementModification: classModification ].
	packageModification := toLoadedPackage compareAgainstBase: fromLoadedPackage.
	classesModification isEmpty
		ifFalse: [ packageModification classesModification: classesModification ].

	packageModification isEmpty
		ifFalse: [ packagesModification addElementModification: packageModification ].

	projectModification := fromLoadedProject compareAgainstBase: toLoadedProject.
	packagesModification isEmpty
		ifFalse: [ projectModification packagesModification: packagesModification ].

	projectModification isEmpty
		ifFalse: [ projectSetModification addElementModification: projectModification ].

	projectSetModification
		updateForPackageMoves;
		updateForClassMoves;
		updateForMethodMoves.
	loadedProjects := Rowan projectTools load
		loadProjectSetModification: projectSetModification
		symbolList: Rowan image symbolList.
	^ (self _loadedClassNamed: className ifAbsent: [  ]) handle currentVersion
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> moveClassNamed: className toPackage: packageName [
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
RwPrjBrowserTool >> moveMethod: methodSelector forClassNamed: className isMeta: isMeta toPackage: packageName [
	"Move the method into <packageName>, whether or not it has been packaged"

	(self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [  ])
		ifNotNil: [ :loadedMethodToBeMoved | 
			| category |
			category := loadedMethodToBeMoved methodCategory.
			loadedMethodToBeMoved loadedProject packageConvention ~= 'Rowan'
				ifTrue: [ 
					"need to fabricate new method protocol"
					loadedMethodToBeMoved loadedClass isLoadedClassExtension
						ifTrue: [ 
							"fabricate new class extension protocol"
							category := '*' , packageName asLowercase ]
						ifFalse: [ 
							"fabricate new non-class extensions protocol"
							category := '(as yet unclassified)' ] ].
			self
				moveMethod: methodSelector
				forClassNamed: className
				isMeta: isMeta
				toProtocol: category
				toPackage: packageName ]
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> moveMethod: methodSelector forClassNamed: className isMeta: isMeta toProtocol: hybridPackageName [
	"hybridPackageName is expected to be an existing protocol in className"

	| loadedMethodToBeMoved srcLoadedClassPackage destinationLoadedPackage |
	loadedMethodToBeMoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [  ].

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
			lc := self _loadedClassNamed: className ifAbsent: [  ].
			lc
				ifNil: [ 
					loadedMethodToBeMoved
						ifNil: [ 
							"unpackaged method moved to unpackaged category"
							^ (Rowan image objectNamed: className)
								_moveMethod: methodSelector
								toCategory: hybridPackageName ].
					RwPerformingUnpackagedEditNotification
						signal:
							'Attempt to move a packaged method to an unpackaged class '
								, className printString
								, '. The unpackaged method will not be tracked by Rowan'.	"Notification resumed, so continue with move"	"Move packaged method to unpackaged category"	"Disown the method, then move it to proper category"
					Rowan packageTools disown
						disownMethod: methodSelector
						inClassNamed: className
						isMeta: isMeta.
					^ (Rowan image objectNamed: className)
						_moveMethod: methodSelector
						toCategory: hybridPackageName ].	"use the loaded package for the class that contains the method"
			srcLoadedClassPackage := lc loadedPackage ].

	loadedMethodToBeMoved
		ifNil: [ 
			| instanceSelectors classSelectors res |
			"Moving an unpackaged method to a package in an unpackage class"
			"Move the method to the proper category and then adopt the method"
			isMeta
				ifTrue: [ 
					instanceSelectors := {}.
					classSelectors := {methodSelector} ]
				ifFalse: [ 
					instanceSelectors := {methodSelector}.
					classSelectors := {} ].
			res := (Rowan image objectNamed: className)
				_moveMethod: methodSelector
				toCategory: hybridPackageName.
			Rowan packageTools adopt
				adoptClassNamed: className
				classExtension: true
				instanceSelectors: instanceSelectors
				classSelectors: classSelectors
				intoPackageNamed: destinationLoadedPackage name.
			^ res ].	"Moving a packaged method to another package"
	self
		moveMethod: methodSelector
		forClassNamed: className
		isMeta: isMeta
		toProtocol: hybridPackageName
		toPackage: destinationLoadedPackage name
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> moveMethod: methodSelector forClassNamed: className isMeta: isMeta toProtocol: protocol toPackage: packageName [
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
			self
				addOrUpdateMethod: loadedMethodToBeMoved source
				dictionaries: Rowan image symbolList
				inProtocol: protocol
				forClassNamed: className
				isMeta: isMeta
				inPackageNamed: packageName ]
]

{ #category : 'modification' }
RwPrjBrowserTool >> packagesModificationForRemoveClass: beforeClass fromPackage: fromPackageDefinition [
	| packageName afterClass toPackageDefinition classModification classesModification packageModification packagesModification |
	packageName := fromPackageDefinition name.
	afterClass := RwClassDefinition new.
	toPackageDefinition := fromPackageDefinition.
	classModification := afterClass compareAgainstBase: beforeClass.
	classesModification := RwClassesModification new.
	classModification isEmpty
		ifFalse: [ classesModification addElementModification: classModification ].
	packageModification := toPackageDefinition
		compareAgainstBase: fromPackageDefinition.
	classesModification isEmpty
		ifFalse: [ packageModification classesModification: classesModification ].
	packagesModification := RwPackagesModification new.
	packageModification isEmpty
		ifFalse: [ packagesModification addElementModification: packageModification ].
	^ packagesModification
]

{ #category : 'modification' }
RwPrjBrowserTool >> packagesModificationForRemoveMethod: fromLoadedMethod fromClass: theLoadedClass isMeta: isMeta fromPackage: fromPackageDefinition [
	| methodModification packageName beforeMethod afterMethod classDefinition className methodsModification toPackageDefinition classModification classesModification packageModification packagesModification |
	packageName := fromPackageDefinition name.
	beforeMethod := fromLoadedMethod asDefinition.
	afterMethod := RwMethodDefinition new.
	classDefinition := theLoadedClass asDefinition.
	className := classDefinition name.
	toPackageDefinition := fromPackageDefinition.
	methodModification := theLoadedClass loadedPackage name = packageName
		ifTrue: [ afterMethod compareAgainstBase: beforeMethod ]
		ifFalse: [ afterMethod compareExtensionMethodsAgainstBase: beforeMethod ].
	methodModification
		isMeta: isMeta;
		classDefinition: classDefinition;
		yourself.
	methodsModification := RwMethodsModification extendedClassName: className.
	methodModification isEmpty
		ifFalse: [ methodsModification addElementModification: methodModification ].
	classModification := classDefinition compareAgainstBase: classDefinition.
	methodsModification isEmpty
		ifFalse: [ 
			isMeta
				ifTrue: [ classModification classMethodsModification: methodsModification ]
				ifFalse: [ classModification instanceMethodsModification: methodsModification ] ].
	classesModification := RwClassesModification new.
	classModification isEmpty
		ifFalse: [ classesModification addElementModification: classModification ].
	packageModification := toPackageDefinition
		compareAgainstBase: fromPackageDefinition.
	classesModification isEmpty
		ifFalse: [ packageModification classesModification: classesModification ].
	packagesModification := RwPackagesModification new.
	packageModification isEmpty
		ifFalse: [ packagesModification addElementModification: packageModification ].
	^ packagesModification
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
	projectDefinition load
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
RwPrjBrowserTool >> removeMethod: methodSelector forClassNamed: className isMeta: isMeta [
	"Remove the given method selector from the class named className"

	| beforeMethod packagesModification fromLoadedMethod theLoadedClass fromPackageDefinition projectModification projectSetModification loadedProjects aSymbolList fromLoadedProject |
	(self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [  ])
		ifNil: [ 
			self
				error:
					'No method ' , methodSelector printString , ' found in the class ',
					(className,
						(isMeta
							ifTrue: [ ' class' ]
							ifFalse: [ '' ])) printString ]
		ifNotNil: [ :loadedMethod | 
			| fromLoadedPackage |
			"removing method in class ...derive definitions ... need beforeMethod and classDefinition"
			fromLoadedMethod := loadedMethod.
			fromLoadedPackage := fromLoadedMethod loadedPackage.
			fromPackageDefinition := fromLoadedPackage asDefinition.
			theLoadedClass := loadedMethod loadedClass.
			fromLoadedProject := fromLoadedPackage loadedProject.
			beforeMethod := fromLoadedMethod asDefinition ].
	projectModification := RwProjectModification
		before: fromLoadedProject
		after: fromLoadedProject.
	packagesModification := self
		packagesModificationForRemoveMethod: fromLoadedMethod
		fromClass: theLoadedClass
		isMeta: isMeta
		fromPackage: fromPackageDefinition.
	projectModification packagesModification: packagesModification.
	projectSetModification := RwProjectSetModification new.
	projectModification isEmpty
		ifFalse: [ projectSetModification addElementModification: projectModification ].
	loadedProjects := Rowan projectTools load
		loadProjectSetModification: projectSetModification
		symbolList: aSymbolList
]

{ #category : 'package browsing' }
RwPrjBrowserTool >> removePackageNamed: packageName [

	| loadedPackage projectDef |
	loadedPackage := self _loadedPackageNamed: packageName.
	projectDef := loadedPackage loadedProject asDefinition.
	projectDef removePackageNamed: loadedPackage name.

	projectDef load
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
RwPrjBrowserTool >> unpackageClass: class [
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
RwPrjBrowserTool >> unpackageClassNamed: className [
	"Unpackage the given class and all methods in the class that in the same package, 
		while leaving the class installed in the image"

	| theClass |
	theClass := Rowan globalNamed: className.
	theClass
		ifNil: [ self error: 'No class named ' , className printString , ' found' ].
	self unpackageClass: theClass
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> unpackageMethod: method [
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
RwPrjBrowserTool >> unpackageMethod: methodSelector forClassNamed: className isMeta: isMeta [
	"unpackage the given method, while leaving the method installed in the image"

	| theBehavior theMethod |
	theBehavior := Rowan globalNamed: className.
	isMeta
		ifTrue: [ theBehavior := theBehavior class ].
	theMethod := theBehavior compiledMethodAt: methodSelector.
	self unpackageMethod: theMethod
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> updateClassCategory: aString forClassNamed: className [
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

	projectDefinition load.
]
