tests
testForceDeleteSimpleProject

	| class projectName packageName className classDefinition symDict registry projectDefinition packageDefinition |

	projectName := 'DeleteProject'.
	packageName := 'DeleteProject-Core'.
	className := 'DeleteProjectClass'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

"create project"
	projectDefinition := (RwProjectDefinition newForGitBasedProjectNamed: projectName)
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
