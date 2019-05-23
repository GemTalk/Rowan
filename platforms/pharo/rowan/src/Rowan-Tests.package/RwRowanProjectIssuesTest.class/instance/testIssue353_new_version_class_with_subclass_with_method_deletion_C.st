tests-issue 353
testIssue353_new_version_class_with_subclass_with_method_deletion_C

	"additional test case (fails as well)"
	"https://github.com/dalehenrich/Rowan/issues/353"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: 'accessing'
					source: 'method5 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super:className1
			instvars: #(ivar1 ivar5)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method4;
		removeClassMethod: #method5;
		yourself.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className2.
	classDefinition
		removeInstanceMethod: #method3;
		removeClassMethod: #method2;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #(ivar0).

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.