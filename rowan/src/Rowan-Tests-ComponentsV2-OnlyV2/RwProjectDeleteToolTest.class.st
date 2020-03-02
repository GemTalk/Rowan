Class {
	#name : 'RwProjectDeleteToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests' }
RwProjectDeleteToolTest >> testDeleteProjectWithExtensionMethods [


	| class projectName1 projectName2 packageName1 packageName2 className1 className2 classDefinition
		symDict registry projectDefinition1 projectDefinition2 packageDefinition projectNames classExtensionDefinition
		projectSetDefinition |

	projectName1 := 'DeleteProject'.
	projectName2 := 'ExtensionProject'.
	packageName1 := 'DeleteProject-Core'.
	packageName2 := 'ExtensionProject-Core'.
	className1 := 'DeleteProjectClass'.
	className2 := 'ExtensionProjectClass'.
	projectNames := {projectName1 . projectName2 }.

	projectNames do: [:projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create projects"
	projectDefinition1 := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName1)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.
	projectDefinition2 := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName2)
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"create classes and methods for project1"
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition;
		addClassExtensionDefinition: classExtensionDefinition.

"create classes and methods for project2"
	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName2
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: classDefinition.

"load project definitions"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1;
		addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: class category= packageName1.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 2.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName1.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class isNil.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwProjectDeleteToolTest >> testDeleteProjectWithExtensionMethods_345 [

	"https://github.com/dalehenrich/Rowan/issues/342"

	| class projectName1 packageName1 packageName2 className1 classDefinition
		symDict registry projectDefinition1 packageDefinition projectNames classExtensionDefinition
		projectSetDefinition |

	projectName1 := 'DeleteProject'.
	packageName1 := 'DeleteProject-Core'.
	packageName2 := 'DeleteProject-Extension'.
	className1 := 'DeleteProjectClass'.
	projectNames := {projectName1  }.

	projectNames do: [:projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create projects"
	projectDefinition1 := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName1)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"create classes and methods for project1"
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition;
		yourself.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition 
		addClassExtensionDefinition: classExtensionDefinition.

"load project definitions"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: class category= packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName1.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwProjectDeleteToolTest >> testDeleteSimpleProject [

	| class projectName packageName className classDefinition symDict registry projectDefinition packageDefinition |

	projectName := 'DeleteProject'.
	packageName := 'DeleteProject-Core'.
	className := 'DeleteProjectClass'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

"create project"
	projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"create classes and methods"
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassDefinition: classDefinition.

"load project definition"
	Rowan projectTools load loadProjectDefinition: projectDefinition.

"validation"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName.
	self assert: class category= packageName.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 1.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName.

"validation"
	class := Rowan globalNamed: className.
	self assert: class isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

]

{ #category : 'tests' }
RwProjectDeleteToolTest >> testForceDeleteProjectWithExtensionMethods [


	| class projectName1 projectName2 packageName1 packageName2 className1 className2 classDefinition
		symDict registry projectDefinition1 projectDefinition2 packageDefinition projectNames classExtensionDefinition
		projectSetDefinition |

	projectName1 := 'DeleteProject'.
	projectName2 := 'ExtensionProject'.
	packageName1 := 'DeleteProject-Core'.
	packageName2 := 'ExtensionProject-Core'.
	className1 := 'DeleteProjectClass'.
	className2 := 'ExtensionProjectClass'.
	projectNames := {projectName1 . projectName2 }.

	projectNames do: [:projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create projects"
	projectDefinition1 := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName1)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.
	projectDefinition2 := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName2)
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"create classes and methods for project1"
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition;
		addClassExtensionDefinition: classExtensionDefinition.

"create classes and methods for project2"
	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName2
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: classDefinition.

"load project definitions"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1;
		addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: class category= packageName1.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 2.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"delete project"
	Rowan projectTools delete forceDeleteProjectNamed: projectName1.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class isNil.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwProjectDeleteToolTest >> testForceDeleteSimpleProject [

	| class projectName packageName className classDefinition symDict registry projectDefinition packageDefinition |

	projectName := 'DeleteProject'.
	packageName := 'DeleteProject-Core'.
	className := 'DeleteProjectClass'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

"create project"
	projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition') newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"create classes and methods"
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassDefinition: classDefinition.

"load project definition"
	Rowan projectTools load loadProjectDefinition: projectDefinition.

"validation"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName.
	self assert: class category= packageName.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 1.

"delete project"
	Rowan projectTools delete forceDeleteProjectNamed: projectName.

"validation"
	class := Rowan globalNamed: className.
	self assert: class isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

]
