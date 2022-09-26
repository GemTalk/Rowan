Class {
	#name : 'RwBrowserToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwBrowserToolTest >> _assert: foundBlock forClassExtensionsIn: className [

	"validate loaded class extension registry structure"

	self
		_assert: foundBlock
		forClassExtensionsIn: className
		ifAbsent: [ 
			self
				assert: false
				description:
					'expected class extension definitions for class ' , className printString
						, ' not found.' ]
]

{ #category : 'private' }
RwBrowserToolTest >> _assert: foundBlock forClassExtensionsIn: className ifAbsent: absentBlock [

	"validate loaded class extension registry structure"

	| projectTools |
	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassExtensionNamed: className
		ifFound: foundBlock
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwBrowserToolTest >> _assert: foundBlock forClassName: className ifAbsent: absentBlock [

	"validate loaded class registry structure"

	| projectTools |
	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassNamed: className
		ifFound: foundBlock
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwBrowserToolTest >> _assert: foundBlock forClassNamed: className [

	"validate loaded class registry structure"

	self
		_assert: foundBlock
		forClassName: className
		ifAbsent: [ 
			self
				assert: false
				description:
					'expected class definitions for class ' , className printString , ' not found.' ]
]

{ #category : 'private' }
RwBrowserToolTest >> _assertForClassExtensionInClassWithHistory: class ifPresent: presentBlock ifAbsent: absentBlock [

	"validate loaded class extension registry structure"

	| found |
	found := IdentitySet new.
	Rowan image symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry loadedClassExtensionsForClass: class ifAbsent: [  ])
						ifNotNil: [ :loadedClassExtensionSet | found addAll: loadedClassExtensionSet ] ] ].
	found isEmpty
		ifFalse: [ ^ presentBlock cull: found ].
	^ absentBlock value
]

{ #category : 'private' }
RwBrowserToolTest >> _assertNoClassExtensionsIn: className [

	"validate loaded class extension registry structure"

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			self
				assert: false
				description:
					'No class extensions expected for class ' , className printString ]
		forClassExtensionsIn: className
		ifAbsent: [  ]
]

{ #category : 'private' }
RwBrowserToolTest >> _basicClassDefinition: className superclass: superclass gs_options: gs_options type: type [

	| def |
	def := RwClassDefinition
		newForClassNamed: className
		super: superclass
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: ''
		comment: ''
		pools: #()
		type: type.
	gs_options ifNil: [ ^ def ].
	(gs_options _isArray and: [ gs_options isEmpty not ])
		ifTrue: [ 
			def properties
				at: #'gs_options'
				put:
					(gs_options collect: [ :each | each asString ]) asSortedCollection asArray ].
	^ def
]

{ #category : 'private' }
RwBrowserToolTest >> _basicClassDefinition: className superclass: superclass type: type [

	^ self
		_basicClassDefinition: className
		superclass: superclass
		gs_options: nil
		type: type
]

{ #category : 'private' }
RwBrowserToolTest >> _issue481_loadProjectDefinition: projectName  packageName1: packageName1 packageName2: packageName2 symDictName: symDictName [

	self
		_loadProjectDefinition: projectName
		packageNames: { packageName1 . packageName2}
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.
]

{ #category : 'private' }
RwBrowserToolTest >> _issue481_projectDefinition: projectName  packageName1: packageName1 packageName2: packageName2 className1: className1 className2: className2 symDictName: symDictName [

	| theClass1 theClass2 symDict instanceMethod1 classMethod1 symbolList 
		instanceMethod2 classMethod2 |
	symbolList := Rowan image symbolList.

	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass1 := Object subclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass1 category: packageName1.

	instanceMethod1 := theClass1
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: #'accessing'
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	classMethod1 := theClass1 class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: #'accessing'
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	Rowan packageTools adopt 
		adoptClassNamed: className1
		intoPackageNamed: packageName1.

	self assert: theClass1 rowanPackageName = packageName1.
	self assert: instanceMethod1 rowanPackageName = packageName1.
	self assert: classMethod1 rowanPackageName = packageName1.

	theClass2 := Object subclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass2 category: packageName2.

	instanceMethod2 := theClass2
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	classMethod2 := theClass2 class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	Rowan packageTools adopt 
		adoptClassExtensionNamed: className2  
		instanceSelectors: #(foo) 
		classSelectors: #(bar)
		intoPackageNamed: packageName2.

	self assert: theClass2 rowanPackageName = Rowan unpackagedName.
	self assert: instanceMethod2 rowanPackageName = packageName2.
	self assert: classMethod2 rowanPackageName = packageName2.

	^(Rowan image loadedProjectNamed: projectName) asDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadExtendedProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName1 extensionPackageNames: extensionPackageNames [

	| projectDefinition classDefinition classExtensionDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment: 'This is a simple project used to test class extension edit API'.

	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName1
		inProject: projectDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1:'
						protocol: 'accessing'
						source: 'ivar1: anInteger ivar1 := anInteger');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'cadd:'
						protocol: 'addition'
						source: 'cadd: anInteger ^civar1 + anInteger');
		yourself.

	projectTools edit
		addClassExtension: classExtensionDefinition
		inPackageNamed: (extensionPackageNames at: 1)
		inProject: projectDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'add:'
						protocol: 'addition'
						source: 'add: anInteger ^ivar1 + anInteger');
		yourself.

	projectTools edit
		addClassExtension: classExtensionDefinition
		inPackageNamed: (extensionPackageNames at: 2)
		inProject: projectDefinition.


	[ projectDefinition load ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
]

{ #category : 'private' }
RwBrowserToolTest >> _loadFullMultiProjectDefinition: projectNames packageNames: packageNames defaultSymbolDictName: defaultSymbolDictNames comment: comments className: classNames inPackageName: classPackageNames [

	| projectTools projectSetDefinition |
	projectTools := Rowan projectTools.

	projectSetDefinition := RwProjectSetDefinition new.

	1 to: projectNames size do: [ :index | 
		| projectDefinition classDefinition |
		(Rowan image loadedProjectNamed: (projectNames at: index) ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

		projectDefinition := self
			_standardProjectDefinition: (projectNames at: index)
			packageNames: (packageNames at: index)
			defaultSymbolDictName: (defaultSymbolDictNames at: index)
			comment:
				'This is a simple project used to work out a "classic" class browser-style API'.

		classDefinition := self _standardClassDefinition: (classNames at: index).
		classDefinition category: (classPackageNames at: index).

		projectTools edit
			addClass: classDefinition
			inPackageNamed: (classPackageNames at: index)
			inProject: projectDefinition.

		projectSetDefinition addProject: projectDefinition.

		index = 1
			ifTrue: [ 
				{(self
					_basicClassDefinition: 'SimpleNormal1'
					superclass: 'Object'
					type: 'normal').
				(self
					_basicClassDefinition: 'SimpleVariable1'
					superclass: 'Object'
					type: 'variable').
				(self
					_basicClassDefinition: 'SimpleBag1'
					superclass: 'Bag'
					gs_options: #('disallowGciStore')
					type: 'normal').
				(self
					_basicClassDefinition: 'SimpleCustomByteArray1'
					superclass: 'Object'
					type: 'byteSubclass').
				(self
					_basicClassDefinition: 'SimpleByteArray1'
					superclass: 'ByteArray'
					type: 'byteSubclass').
				(self _basicClassDefinition: 'SimpleProto1' superclass: 'nil' type: 'normal')}
					do: [ :cd | 
						cd category: (classPackageNames at: index).
						projectTools edit
							addClass: cd
							inPackageNamed: (classPackageNames at: index)
							inProject: projectDefinition ] ] ].

	[ projectTools load loadProjectSetDefinition: projectSetDefinition  ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
]

{ #category : 'private' }
RwBrowserToolTest >> _loadGlobalExtensionsProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_globalExtensionsProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	projectDefinition load
]

{ #category : 'private' }
RwBrowserToolTest >> _loadMultiProjectDefinition: projectNames packageNames: packageNames defaultSymbolDictName: defaultSymbolDictNames comment: comments className: classNames inPackageName: classPackageNames [

	| projectTools projectSetDefinition |
	projectTools := Rowan projectTools.

	projectSetDefinition := RwProjectSetDefinition new.

	1 to: projectNames size do: [ :index | 
		| projectDefinition classDefinition |
		(Rowan image loadedProjectNamed: (projectNames at: index) ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

		projectDefinition := self
			_standardProjectDefinition: (projectNames at: index)
			packageNames: (packageNames at: index)
			defaultSymbolDictName: (defaultSymbolDictNames at: index)
			comment:
				'This is a simple project used to work out a "classic" class browser-style API'.

		classDefinition := self _standardClassDefinition: (classNames at: index).
		classDefinition category: (classPackageNames at: index).

		projectTools edit
			addClass: classDefinition
			inPackageNamed: (classPackageNames at: index)
			inProject: projectDefinition.

		projectSetDefinition addProject: projectDefinition ].

	[ projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
]

{ #category : 'private' }
RwBrowserToolTest >> _loadPackageMappedProjectDefinition: projectName packageNameMap: packageNameMap defaultSymbolDictName: defaultSymbolDictName [
	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: defaultSymbolDictName.

	projectDefinition load
]

{ #category : 'private' }
RwBrowserToolTest >> _loadProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	projectDefinition load.

	^ projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadSessionMethodExtensionProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		defaultUseSessionMethodsForExtensions: true
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	projectDefinition load
]

{ #category : 'private' }
RwBrowserToolTest >> _loadSimpleProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName [

	| projectDefinition classDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	classDefinition := self _simpleClassDefinition: className.
	classDefinition category: packageName.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectDefinition load
]

{ #category : 'private' }
RwBrowserToolTest >> _loadStandardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName [

	| projectDefinition classDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	classDefinition := self _standardClassDefinition: className.
	classDefinition category: packageName.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	[ projectDefinition load ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
]

{ #category : 'private' }
RwBrowserToolTest >> _simpleClassDefinition: className [

	^ RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'
]

{ #category : 'private' }
RwBrowserToolTest >> _standardClassDefinition: className [

	| classDefinition |
	classDefinition := self _simpleClassDefinition: className.

	classDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1. Cvar1 := 2.');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'civar1'
						protocol: 'accessing'
						source: 'civar1 ^civar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'cvar1'
						protocol: 'accessing'
						source: 'cvar1 ^Cvar1');
		yourself.

	^ classDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	^ self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		defaultUseSessionMethodsForExtensions: false
		comment: comment
]
