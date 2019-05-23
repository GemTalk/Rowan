tests
testDeleteProjectWithExtensionMethods_345

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
	projectDefinition1 := (RwProjectDefinition newForGitBasedProjectNamed: projectName1)
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