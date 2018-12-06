Class {
	#name : 'RwProjectSetTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectSetTest >> testProjectAdoptTool [

	"Create project definition structure (2 projects), build classes and methods using traditional methods, 
		then adopt the structure"

	| projectTools projectName1 projectName2 projectDefinition1 projectDefinition2 projectSetDefinition projectNames
		classDefinition packageNames1 packageNames2 classNames1 classNames2 classExtensionDefinition 
		symDict1 symDict2 symbolList theClass loadedProjectSet diff classComment |

	projectTools := Rowan projectTools.
	projectName1 := 'AdoptProject1'.
	projectName2 := 'AdoptProject2'.
	projectNames := {projectName1.
		projectName2}.
	packageNames1 := #('Adopt1-Core' 'Adopt1-Extensions').
	packageNames2 := #('Adopt2-Core' 'Adopt2-Extensions').
	classNames1 := #('Adopt1Class1').
	classNames2 := #('Adopt2Class1').
	classComment := ''.

	projectNames
		do: [ :projectName | 
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName1.
	projectDefinition1
		packageNames: packageNames1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: (packageNames1 at: 2).

	projectSetDefinition addDefinition: projectDefinition1.

	projectDefinition2 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName2.
	projectDefinition2
		packageNamesInLoadOrder: packageNames2;
		defaultSymbolDictName: self _symbolDictionaryName2;
		setSymbolDictName: self _symbolDictionaryName1 forPackageNamed: (packageNames2 at: 2).
	projectSetDefinition addDefinition: projectDefinition2.

	projectTools load loadProjectSetDefinition: projectSetDefinition. "create loaded project and empty loaded packages"

	"construct project definition structures"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := (Rowan image loadedProjectNamed: projectName1) asDefinition.
	projectSetDefinition addProject: projectDefinition1.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames1 at: 1)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: (packageNames1 at: 1)
		comment: classComment
		pools: #()
		type: 'normal'.
	classDefinition
		gs_symbolDictionary: self _symbolDictionaryName1;
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'instanceFoo'
				protocol: 'accessing'
				source: 'instanceFoo ^true');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'classFoo'
				protocol: 'accessing'
				source: 'classFoo ^true').

	(projectDefinition1 packageNamed: (packageNames1 at: 1))
		addClassDefinition: classDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition
			newForClassNamed: (classNames2 at: 1))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceExtensionFoo'
						protocol: '*', (packageNames1 at: 2)
						source: 'instanceExtensionFoo ^true');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classExtensionFoo'
						protocol:'*', (packageNames1 at: 2)
						source: 'classExtensionFoo ^true');		
		yourself.

	(projectDefinition1 packageNamed: (packageNames1 at: 2))
		addClassExtension: classExtensionDefinition.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName2) asDefinition.
	projectSetDefinition addProject: projectDefinition2.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames2 at: 1)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: (packageNames2 at: 1)
		comment: classComment
		pools: #()
		type: 'normal'.
	classDefinition
		gs_symbolDictionary: self _symbolDictionaryName2;
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'instanceBar'
				protocol: 'accessing'
				source: 'instanceBar ^true');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'classBar'
				protocol: 'accessing'
				source: 'classBar ^true').

	(projectDefinition2 packageNamed: (packageNames2 at: 1))
		addClassDefinition: classDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition
			newForClassNamed: (classNames1 at: 1))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceExtensionBar'
						protocol: '*', (packageNames2 at: 2)
						source: 'instanceExtensionBar ^true');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classExtensionBar'
						protocol:'*', (packageNames2 at: 2)
						source: 'classExtensionBar ^true');		
		yourself.

	(projectDefinition2 packageNamed: (packageNames2 at: 2))
		addClassExtension: classExtensionDefinition.

	"construct class structures using traditional methods"
	symbolList := Rowan image symbolList.

	symDict1 := Rowan globalNamed: self _symbolDictionaryName1.
	theClass := Object subclass: (classNames1 at: 1)
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: symDict1
			options: #().
	theClass 
		comment: classComment;
		category: (packageNames1 at: 1).
	theClass
		compileMethod: 'instanceFoo ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'instanceExtensionBar ^true'
			dictionaries: symbolList
			category: '*', (packageNames2 at: 2).
	theClass class
		compileMethod: 'classFoo ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'classExtensionBar ^true'
			dictionaries: symbolList
			category: '*', (packageNames2 at: 2).

	symDict2 := Rowan globalNamed: self _symbolDictionaryName2.
	theClass := Object subclass: (classNames2 at: 1)
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: symDict2
			options: #().
	theClass 
		comment: classComment;
		category: (packageNames2 at: 1).
	theClass
		compileMethod: 'instanceBar ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'instanceExtensionFoo ^true'
			dictionaries: symbolList
			category: '*', (packageNames1 at: 2).
	theClass class
		compileMethod: 'classBar ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'classExtensionFoo ^true'
			dictionaries: symbolList
			category: '*', (packageNames1 at: 2).

	"adopt the class structure using the project definitions"
	projectTools adopt adoptProjectSetDefinition: projectSetDefinition.

	"validate that adopted project structure matches the project defintions"
	loadedProjectSet := projectSetDefinition deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinition compareAgainstBase: loadedProjectSet.
	self assert: diff isEmpty.
]

{ #category : 'tests' }
RwProjectSetTest >> testProjectAdoptTool_existing_project [

	"Create project definition structure (2 projects), build classes and methods using traditional methods, 
		then adopt the structure"

	"test for RwPkgCreateTool>>createLoadedPackageNamed:inProjectNamed:"

	| projectTools projectName1 projectName2 projectDefinition1 projectDefinition2 projectSetDefinition projectNames
		classDefinition packageNames1 packageNames2 classNames1 classNames2 classExtensionDefinition 
		symDict1 symDict2 symbolList theClass loadedProjectSet diff classComment pkgCreateTool |

	projectTools := Rowan projectTools.
	projectName1 := 'AdoptProject1'.
	projectName2 := 'AdoptProject2'.
	projectNames := {projectName1.
		projectName2}.
	packageNames1 := #('Adopt1-Core' 'Adopt1-Extensions').
	packageNames2 := #('Adopt2-Core' 'Adopt2-Extensions').
	classNames1 := #('Adopt1Class1').
	classNames2 := #('Adopt2Class1').
	classComment := ''.

	projectNames
		do: [ :projectName | 
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName1.
	projectDefinition1
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: (packageNames1 at: 2).

	projectSetDefinition addDefinition: projectDefinition1.

	projectDefinition2 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName2.
	projectDefinition2
		defaultSymbolDictName: self _symbolDictionaryName2;
		setSymbolDictName: self _symbolDictionaryName1 forPackageNamed: (packageNames2 at: 2).
	projectSetDefinition addDefinition: projectDefinition2.

	"create projects with no packages"
	projectTools load loadProjectSetDefinition: projectSetDefinition. "create loaded project and empty loaded packages"

	"add new packages to loaded projects"
	pkgCreateTool := Rowan packageTools create.
	
	packageNames1 do: [:packageName |
		pkgCreateTool createLoadedPackageNamed: packageName inProjectNamed: projectName1 ].
	packageNames2 do: [:packageName |
		pkgCreateTool createLoadedPackageNamed: packageName inProjectNamed: projectName2 ].

	"construct project definition structures"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := (Rowan image loadedProjectNamed: projectName1) asDefinition.
	projectSetDefinition addProject: projectDefinition1.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames1 at: 1)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: (packageNames1 at: 1)
		comment: classComment
		pools: #()
		type: 'normal'.
	classDefinition
		gs_symbolDictionary: self _symbolDictionaryName1;
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'instanceFoo'
				protocol: 'accessing'
				source: 'instanceFoo ^true');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'classFoo'
				protocol: 'accessing'
				source: 'classFoo ^true').

	(projectDefinition1 packageNamed: (packageNames1 at: 1))
		addClassDefinition: classDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition
			newForClassNamed: (classNames2 at: 1))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceExtensionFoo'
						protocol: '*', (packageNames1 at: 2)
						source: 'instanceExtensionFoo ^true');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classExtensionFoo'
						protocol:'*', (packageNames1 at: 2)
						source: 'classExtensionFoo ^true');		
		yourself.

	(projectDefinition1 packageNamed: (packageNames1 at: 2))
		addClassExtension: classExtensionDefinition.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName2) asDefinition.
	projectSetDefinition addProject: projectDefinition2.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames2 at: 1)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: (packageNames2 at: 1)
		comment: classComment
		pools: #()
		type: 'normal'.
	classDefinition
		gs_symbolDictionary: self _symbolDictionaryName2;
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'instanceBar'
				protocol: 'accessing'
				source: 'instanceBar ^true');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'classBar'
				protocol: 'accessing'
				source: 'classBar ^true').

	(projectDefinition2 packageNamed: (packageNames2 at: 1))
		addClassDefinition: classDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition
			newForClassNamed: (classNames1 at: 1))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceExtensionBar'
						protocol: '*', (packageNames2 at: 2)
						source: 'instanceExtensionBar ^true');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classExtensionBar'
						protocol:'*', (packageNames2 at: 2)
						source: 'classExtensionBar ^true');		
		yourself.

	(projectDefinition2 packageNamed: (packageNames2 at: 2))
		addClassExtension: classExtensionDefinition.

	"construct class structures using traditional methods"
	symbolList := Rowan image symbolList.

	symDict1 := Rowan globalNamed: self _symbolDictionaryName1.
	theClass := Object subclass: (classNames1 at: 1)
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: symDict1
			options: #().
	theClass 
		comment: classComment;
		category: (packageNames1 at: 1).
	theClass
		compileMethod: 'instanceFoo ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'instanceExtensionBar ^true'
			dictionaries: symbolList
			category: '*', (packageNames2 at: 2).
	theClass class
		compileMethod: 'classFoo ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'classExtensionBar ^true'
			dictionaries: symbolList
			category: '*', (packageNames2 at: 2).

	symDict2 := Rowan globalNamed: self _symbolDictionaryName2.
	theClass := Object subclass: (classNames2 at: 1)
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: symDict2
			options: #().
	theClass 
		comment: classComment;
		category: (packageNames2 at: 1).
	theClass
		compileMethod: 'instanceBar ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'instanceExtensionFoo ^true'
			dictionaries: symbolList
			category: '*', (packageNames1 at: 2).
	theClass class
		compileMethod: 'classBar ^true'
			dictionaries: symbolList
			category: 'accessing';
		compileMethod: 'classExtensionFoo ^true'
			dictionaries: symbolList
			category: '*', (packageNames1 at: 2).

	"adopt the class structure using the project definitions"
	projectTools adopt adoptProjectSetDefinition: projectSetDefinition.

	"validate that adopted project structure matches the project defintions"
	loadedProjectSet := projectSetDefinition deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinition compareAgainstBase: loadedProjectSet.
	self assert: diff isEmpty.

]

{ #category : 'tests' }
RwProjectSetTest >> testProjectSetLoad1 [

	| projectTools projectName1 projectName2 projectDefinition1 projectDefinition2 projectSetDefinition projectNames loadedProjects |
	projectTools := Rowan projectTools.
	projectName1 := 'Simple1'.
	projectName2 := 'Simple2'.
	projectNames := {projectName1.
	projectName2}.


	projectNames
		do: [ :projectName | 
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName1.
	projectDefinition1
		packageNames: #('Simple1-Core');
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectSetDefinition addDefinition: projectDefinition1.

	projectDefinition2 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName2.
	projectDefinition2
		packageNamesInLoadOrder: #('Simple2-Core');
		defaultSymbolDictName: self _symbolDictionaryName2.
	projectSetDefinition addDefinition: projectDefinition2.

	projectTools load loadProjectSetDefinition: projectSetDefinition.

	self
		assert:
			(loadedProjects := self _loadedProjectsNamed: projectNames) size
				= projectNames size.
	loadedProjects entities
		do: [ :loadedProject | self assert: (projectNames includes: loadedProject name) ]
]
