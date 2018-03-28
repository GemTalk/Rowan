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
RwBrowserToolTest >> _assertForClassExtensionInClassWithHistory: classHistory ifPresent: presentBlock ifAbsent: absentBlock [

	"validate loaded class extension registry structure"

	| found |
	found := IdentitySet new.
	Rowan image symbolList
		do: [ :symbolDict | 
			(symbolDict isKindOf: RwGsPackageSymbolDictionary)
				ifTrue: [ 
					(symbolDict classExtensionRegistry at: classHistory ifAbsent: [  ])
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
				at: 'gs_options'
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
RwBrowserToolTest >> _globalExtensionsProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitPackageProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNamesInLoadOrder: packageNames;
		defaultSymbolDictName: defaultSymbolDictName;
		yourself.

	packageNames
		do: [ :packageName | 
			projectDefinition
				useSessionMethodsForExtensions: true
				forPackageNamed: packageName ].

	^ projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadExtendedProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName1 extensionPackageNames: extensionPackageNames [

	| projectDefinition classDefinition classExtensionDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

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


	projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadFullMultiProjectDefinition: projectNames packageNames: packageNames defaultSymbolDictName: defaultSymbolDictNames comment: comments className: classNames inPackageName: classPackageNames [

	| projectTools projectSetDefinition |
	projectTools := Rowan projectTools.

	projectSetDefinition := RwProjectSetDefinition new.

	1 to: projectNames size do: [ :index | 
		| projectDefinition classDefinition |
		(Rowan loadedProjectNamed: (projectNames at: index) ifAbsent: [  ])
			ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

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
					type: 'bytes').
				(self
					_basicClassDefinition: 'SimpleByteArray1'
					superclass: 'ByteArray'
					type: 'bytes').
				(self _basicClassDefinition: 'SimpleProto1' superclass: 'nil' type: 'normal')}
					do: [ :cd | 
						cd category: (classPackageNames at: index).
						projectTools edit
							addClass: cd
							inPackageNamed: (classPackageNames at: index)
							inProject: projectDefinition ] ] ].

	projectTools load loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadGlobalExtensionsProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

	projectDefinition := self
		_globalExtensionsProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.


	projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadMultiProjectDefinition: projectNames packageNames: packageNames defaultSymbolDictName: defaultSymbolDictNames comment: comments className: classNames inPackageName: classPackageNames [

	| projectTools projectSetDefinition |
	projectTools := Rowan projectTools.

	projectSetDefinition := RwProjectSetDefinition new.

	1 to: projectNames size do: [ :index | 
		| projectDefinition classDefinition |
		(Rowan loadedProjectNamed: (projectNames at: index) ifAbsent: [  ])
			ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

		projectDefinition := self
			_standardProjectDefinition: (projectNames at: index)
			packageNames: (packageNames at: index)
			defaultSymbolDictName: (defaultSymbolDictNames at: index)
			comment:
				'This is a simple project used to work out a "classic" class browser-style API'.

		classDefinition := self _standardClassDefinition: (classNames at: index).

		projectTools edit
			addClass: classDefinition
			inPackageNamed: (classPackageNames at: index)
			inProject: projectDefinition.

		projectSetDefinition addProject: projectDefinition ].

	projectTools load loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition classDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadSimpleProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName [

	| projectDefinition classDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	classDefinition := self _simpleClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _loadStandardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName [

	| projectDefinition classDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition
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

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitPackageProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNamesInLoadOrder: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition
]
