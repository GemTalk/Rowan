Class {
	#name : 'RwBrowserToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwBrowserToolTest >> _expectedClassCreationTemplate [

	^ 'RwClassDefinition
	newForClassNamed: ''NameOfSubclass''
	 super: Object
	 instvars: #() 
	 classinstvars: #() 
	 classvars: #() 
	 category: #() 
	 comment: ''class comment'' 
	 pools: #() 
	 type: ''normal|variable|bytes'''
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
		defaultSymbolDictName: self _symbolDictionaryName1
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


	^ projectTools load loadProjectDefinition: projectDefinition
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
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	^ projectTools load loadProjectDefinition: projectDefinition
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
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	^ projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'private' }
RwBrowserToolTest >> _standardClassDefinition: className [

	| classDefinition |
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'.

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

{ #category : 'tests' }
RwBrowserToolTest >> testAddMethod [

	| projectName packageNames className packageName1 packageName2 testClass testInstance browserTool |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	browserTool := Rowan projectTools browser.

	browserTool
		addMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance ivar1: 3.
	self assert: testInstance ivar1 = 3.
	self should: [ testInstance foo ] raise: MessageNotUnderstood.

	browserTool
		addMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.

	self assert: testInstance foo = 'bar'.

	browserTool
		addMethod: 'foo ^''baz'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.

	self assert: testInstance foo = 'baz'
]

{ #category : 'tests' }
RwBrowserToolTest >> testClassCreationTemplate [

	| template |
	template := Rowan projectTools browser
		classCreationTemplateForSubclassOf: 'Object'
		category: 'MyClasses'.
	self
		assert: template = self _expectedClassCreationTemplate
		description: 'incorrect class creation template'
]

{ #category : 'tests' }
RwBrowserToolTest >> testCreateClass [

	| projectName packageNames className packageName classDefinition browserTool testClass |
	projectName := 'Simple Browser'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	className := 'SimpleBrowse'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil
]
