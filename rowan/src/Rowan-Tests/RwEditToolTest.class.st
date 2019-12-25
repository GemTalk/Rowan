Class {
	#name : 'RwEditToolTest',
	#superclass : 'RwToolTest',
	#instVars : [
		'globalBlackList',
		'userBlackList',
		'sessionBlackList'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwEditToolTest >> _loadExtendedProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName1 extensionPackageName: packageName2 [

	| projectDefinition classDefinition classExtensionDefinition projectTools |
	projectTools := Rowan projectTools.

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
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'add:'
						protocol: 'addition'
						source: 'add: anInteger ^ivar1 + anInteger');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'cadd:'
						protocol: 'addition'
						source: 'cadd: anInteger ^civar1 + anInteger');
		yourself.

	projectTools edit
		addClassExtension: classExtensionDefinition
		inPackageNamed: packageName2
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

]

{ #category : 'private' }
RwEditToolTest >> _loadExtendedProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName1 extensionPackageNames: extensionPackageNames [

	| projectDefinition classDefinition classExtensionDefinition projectTools |
	projectTools := Rowan projectTools.

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


	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

]

{ #category : 'private' }
RwEditToolTest >> _loadStandardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName [

	| projectDefinition classDefinition projectTools |
	projectTools := Rowan projectTools.

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

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

]

{ #category : 'private' }
RwEditToolTest >> _standardClassDefinition: className [

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
RwEditToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNames: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition
]

{ #category : 'running' }
RwEditToolTest >> setUp [

	| preferenceSymbol |
	super setUp.

"get current black list values"
	preferenceSymbol := Rowan platform _automaticClassInitializationBlackList_symbol.
	globalBlackList :=	(Rowan platform 
		globalPreferenceFor: preferenceSymbol 
		ifAbsent: []) copy.
	userBlackList :=	(Rowan platform 
		userPreferenceFor: preferenceSymbol 
		ifAbsent: []) copy.
	sessionBlackList :=	(Rowan platform 
		sessionPreferenceFor: preferenceSymbol 
		ifAbsent: []) copy.
]

{ #category : 'running' }
RwEditToolTest >> tearDown [

	| preferenceSymbol |

	super tearDown.

"clean up blackList"
	preferenceSymbol := Rowan platform _automaticClassInitializationBlackList_symbol.
	globalBlackList
		ifNil: [ Rowan clearDefaultAutomaticClassInitializationBlackList ]
		ifNotNil: [
			Rowan platform 
				setDefaultPreferenceFor: preferenceSymbol 
				to: globalBlackList ].
	userBlackList
		ifNil: [ Rowan clearUserAutomaticClassInitializationBlackList ]
		ifNotNil: [
			Rowan platform 
				setUserPreferenceFor: preferenceSymbol 
				to: userBlackList ].
	sessionBlackList
		ifNil: [ Rowan clearSessionAutomaticClassInitializationBlackList ]
		ifNotNil: [
			Rowan platform 
				setSessionPreferenceFor: preferenceSymbol 
				to: sessionBlackList ].
]

{ #category : 'tests - classes' }
RwEditToolTest >> testAddAndRemoveClass [

	| projectName projectDefinition projectTools packageNames classDefinition packageName testClass testInstance className |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	className := 'SimpleEdit'.
	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.


	projectTools edit
		removeClassNamed: className
		fromPackageNamed: packageName
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	testClass := Rowan globalNamed: className.
	self assert: testClass isNil
]

{ #category : 'tests - classes' }
RwEditToolTest >> testAddClass_blackList_A [

	"https://github.com/GemTalk/Rowan/issues/447"

	"test that blackList is used to block class initialization - sessionAutomaticClassInitializationBlackList"

	| projectName projectDefinition projectTools packageNames classDefinition packageName testClass testInstance className |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create project definition"
	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'Testing class initialization blackList'.

	className := 'SimpleEdit'.
	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

"add project to session blackList"
	Rowan sessionAutomaticClassInitializationBlackList add: projectName.

"load project - initialization should not be triggered"
	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | self assert: false description: 'unexpected signal of RwExecuteClassInitializeMethodsAfterLoadNotification'  ].

"validate"
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) isNil.
	self assert: (testClass perform: #cvar1) isNil.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.
]

{ #category : 'tests - classes' }
RwEditToolTest >> testAddClass_blackList_B [

	"https://github.com/GemTalk/Rowan/issues/447"

	"test that blackList is used to block class initialization - userAutomaticClassInitializationBlackList"

	| projectName projectDefinition projectTools packageNames classDefinition packageName testClass testInstance className |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create project definition"
	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'Testing class initialization blackList'.

	className := 'SimpleEdit'.
	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

"add project to user blackList"
	Rowan userAutomaticClassInitializationBlackList add: projectName.

"load project - initialization should not be triggered"
	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | self assert: false description: 'unexpected signal of RwExecuteClassInitializeMethodsAfterLoadNotification'  ].

"validate"
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) isNil.
	self assert: (testClass perform: #cvar1) isNil.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.
]

{ #category : 'tests - classes' }
RwEditToolTest >> testAddClassExtension [

	| projectName packageNames packageName1 packageName2 className testClass testInstance |
	projectName := 'SimpleClassExtension'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extension'.
	packageNames := {packageName1.
	packageName2}.
	className := 'SimpleEdit'.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadExtendedProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to illustrate how to edit an existing class.'
		className: className
		inPackageName: packageName1
		extensionPackageName: packageName2.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cadd: with: 1) = 2.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	(testInstance perform: #ivar1: with: 2).
	self assert: (testInstance perform: #ivar1) = 2.
	self assert: (testInstance add: 1) = 3
]

{ #category : 'tests - classes' }
RwEditToolTest >> testAddUpdateAndRemoveClass [

	| projectName projectDefinition projectTools packageNames classDefinition packageName testClass testInstance className |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	className := 'SimpleEdit'.
	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.

	classDefinition
		instVarNames: #();
		removeInstanceMethod: #'ivar1';
		classVarNames: #();
		removeClassMethod: #'cvar1';
		updateClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1.');
		yourself.

	projectTools edit
		updateOrAddClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self should: [ testClass perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testClass perform: #civar1) = 1.
	testInstance := testClass new.
	self should: [ testInstance perform: #ivar1 ] raise: MessageNotUnderstood.

	projectTools edit
		removeClass: classDefinition
		fromPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass := Rowan globalNamed: className.
	self assert: testClass isNil
]

{ #category : 'test api' }
RwEditToolTest >> testPreferences [

	| projectName |
	projectName := '__Test-Preferences__'.

"standard API - user in GemStone"
	Rowan automaticClassInitializationBlackList add: projectName.
	self assert: (Rowan automaticClassInitializationBlackList includes: projectName).

	Rowan clearAutomaticClassInitializationBlackList.
	self deny: (Rowan automaticClassInitializationBlackList includes: projectName).

"globals"
	Rowan globalAutomaticClassInitializationBlackList add: projectName.
	self assert: (Rowan globalAutomaticClassInitializationBlackList includes: projectName).

	Rowan clearGlobalAutomaticClassInitializationBlackList.
	self deny: (Rowan globalAutomaticClassInitializationBlackList includes: projectName).

"users"
	Rowan userAutomaticClassInitializationBlackList add: projectName.
	self assert: (Rowan userAutomaticClassInitializationBlackList includes: projectName).

	Rowan clearUserAutomaticClassInitializationBlackList.
	self deny: (Rowan userAutomaticClassInitializationBlackList includes: projectName).

"session"
	Rowan  sessionAutomaticClassInitializationBlackList add: projectName.
	self assert: (Rowan sessionAutomaticClassInitializationBlackList includes: projectName).

	Rowan clearSessionAutomaticClassInitializationBlackList.
	self deny: (Rowan sessionAutomaticClassInitializationBlackList includes: projectName).
]

{ #category : 'tests - classes' }
RwEditToolTest >> testUpdateClass [

	| projectName packageNames packageName className projectTools projectDefinition classDefinition testClass testInstance |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	className := 'SimpleEdit'.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to illustrate how to edit an existing class.'
		className: className
		inPackageName: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.

	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			projectDefinition := projectDef.
			classDefinition := classDef ]
		ifAbsent: [ 
			self
				assert: false
				description:
					'expected class definition for class ' , className printString , ' not found.' ].

	classDefinition
		instVarNames: #();
		removeInstanceMethod: #'ivar1';
		classVarNames: #();
		removeClassMethod: #'cvar1';
		updateClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1.');
		yourself.

	projectTools edit
		updateOrAddClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self should: [ testClass perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testClass perform: #civar1) = 1.
	testInstance := testClass new.
	self should: [ testInstance perform: #ivar1 ] raise: MessageNotUnderstood
]

{ #category : 'tests - classes' }
RwEditToolTest >> testUpdateClassExtension [

	| projectName packageNames packageName1 packageName2 packageName3 className projectTools testClass testInstance classExtensionDefinitions |
	projectName := 'SimpleClassExtension'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extension1'.
	packageName3 := 'Simple-Extension2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className := 'SimpleEdit'.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadExtendedProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to illustrate how to edit an existing class.'
		className: className
		inPackageName: packageName1
		extensionPackageNames:
			{packageName2.
			packageName3}.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cadd: with: 1) = 2.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	testInstance perform: #ivar1: with: 2.
	self assert: (testInstance perform: #ivar1) = 2.
	self assert: (testInstance add: 1) = 3.

	classExtensionDefinitions := {}.
	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassExtensionNamed: className
		ifFound: [ :classExtensionDef :packageDef :projectDef | 
			classExtensionDefinitions
				add:
					{classExtensionDef.
					packageDef.
					projectDef} ]
		ifAbsent: [ 
			self
				assert: false
				description:
					'expected class extension definitions for class ' , className printString
						, ' not found.' ].
	self
		assert: classExtensionDefinitions size = 2
		description:
			'two packages contain extensions for this class ... classExtensionRegistry does not record multiple extensions correctly'.

	classExtensionDefinitions
		do: [ :ar | 
			| classExtensionDef packageDef projectDef |
			classExtensionDef := ar at: 1.
			packageDef := ar at: 2.
			projectDef := ar at: 3.
			self assert: projectDef name = projectName.
			self assert: (packageNames includes: packageDef name).
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert:
								(classExtensionDef instanceMethodDefinitions includesKey: #'ivar1:');
						assert: classExtensionDef classMethodDefinitions size = 1;
						assert: (classExtensionDef classMethodDefinitions includesKey: #'cadd:') ].
			packageDef name = packageName3
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'add:') ] ]
]
