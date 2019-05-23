tests-issue 393
testIssue393_new_version_class_with_subclass_with_extension_method_deletion_A

	"new test case:superclass has new class version ... the subclass has an extension method removed --- simplest case"
	"https://github.com/dalehenrich/Rowan/issues/393"

	| projectName  packageName1 packageName2 projectDefinition classDefinition packageDefinition className1  
		projectSetDefinition audit classExtensionDefinition |

	projectName := 'Issue326'.
	packageName1 := 'Issue326-Core'.
	packageName2 := 'Issue326-Extension'.
	className1 := 'Issue326Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #(ivar0)
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

	packageDefinition 
		addClassDefinition: classDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1').

	packageDefinition 
		addClassExtensionDefinition: classExtensionDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove extension method"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at: className1.

	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #().

	classExtensionDefinition := (projectDefinition packageNamed: packageName2) classExtensions at:className1.
	classExtensionDefinition
		removeInstanceMethod: #method1;
		yourself.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.