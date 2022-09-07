Extension { #name : 'RwRowanSample9Test' }

{ #category : '*rowan-tests-gemstone-36x' }
RwRowanSample9Test >> testIssue345 [

	"ported from RwRowanSample1Test>>testIssue345"

	"https://github.com/GemTalk/Rowan/issues/514"

	"This test (for 3.5.0) is actually about being able to change the component
		properties before loading ... post clone ... Right now I am not completely
		sure what sematics should be applied:
			- disallow changing component properteis - if you want different properties
				edit the component --- NOT COOL
			- somehow arrange for changes made to the ""loaded project"" to survive
				the read that ??must happend?? before being able to load the project 
				from disk
			- require that the project be read from disk before making changes and
				then somehow arranging to do a load of the project in memory and avoid
				re-reading from disk ... THIS PROBABLY THE RIGHT ANSWER ... need to 
				get second and third opinions on this one ... and expand test coverage,
				I assume that there are existing tests (the extension methods in this 
				package are good clues) that have hacked some sort of behavior in this
				area, but we need a REAL solution with some logic behind it more than
				""this is how it works:)"""

	"testIssue514, thoroughly tests that the change attributs are being preserved"

	| loadSpec projectName resolvedProject loadedProjects symDict registry audit |
	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName; "different than originally specified"
		yourself.

	resolvedProject repository checkout: 'spec_0033'.

"explicitly read project, since the disk version has changed. then load project"
	loadedProjects := resolvedProject
		read;
		load.

"validate"
	loadedProjects 
		do: [:project |  
			self assert: (audit := project audit) isEmpty].
	self _validate: resolvedProject loadedProjects: loadedProjects expectedTestErrors: #(#'testSymbolDicts').

"issue 345 validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 3.

"unload (delete) project"
	(Rowan projectNamed: projectName) unload.

"issue 345 validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwRowanSample9Test >> testIssue586_move_class_to_symbolDict_1 [

	"https://github.com/dalehenrich/Rowan/issues/586"

	"move RowanSample9Class from symbol dictionary RowanSample9_1 to _sampleSymbolDictionaryName2, changing project default symbol dictionary name"

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className class symDict registry |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1.
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.
	className :=projectName , 'Class1'.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name equals: self _sampleSymbolDictionaryName1.


"load project using symbol dict  _sampleSymbolDictionaryName2"
	resolvedProject := project asDefinition.
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.
	loadedProjects := resolvedProject load.

"validate"
	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name equals: self _sampleSymbolDictionaryName2.
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwRowanSample9Test >> testIssue586_move_class_to_symbolDict_2 [

	"https://github.com/dalehenrich/Rowan/issues/586"

	"move RowanSample9Class from symbol dictionary RowanSample9_1 to _sampleSymbolDictionaryName2, changing project default symbol dictionary name
		AND changing class version (load spec_0034)"

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className class symDict registry |
UserGlobals at: #ConditionalHalt put: false.
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1.
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.
	className := projectName , 'Class1'.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name equals: self _sampleSymbolDictionaryName1.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.

"load project using symbol dict  _sampleSymbolDictionaryName2"
	resolvedProject := project asDefinition.
	resolvedProject 
		checkout: 'spec_0034';
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.
UserGlobals at: #ConditionalHalt put: false.
	loadedProjects := resolvedProject load.

"validate"
	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name equals: self _sampleSymbolDictionaryName2.

	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwRowanSample9Test >> testIssue586_move_class_to_symbolDict_3 [

	"https://github.com/dalehenrich/Rowan/issues/586"

	"move RowanSample9Class from symbol dictionary RowanSample9_1 to _sampleSymbolDictionaryName2, changing PACKAGE default symbol dictionary name"

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className class x 
		packageName symDict registry |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	packageName := projectName, '-Core'.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName1 forPackageNamed: packageName.
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.
	className := projectName, 'Class1'.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName1.

"load project using symbol dict  _sampleSymbolDictionaryName2"
	resolvedProject := project asDefinition.
	resolvedProject gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName.
	loadedProjects := resolvedProject load.

"validate"
	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _sampleSymbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
]
