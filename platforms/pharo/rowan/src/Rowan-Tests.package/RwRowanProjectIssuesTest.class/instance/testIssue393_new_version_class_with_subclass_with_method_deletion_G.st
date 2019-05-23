tests-issue 393
testIssue393_new_version_class_with_subclass_with_method_deletion_G

	"new test case:superclass has new class version ... the subclass has a method removed --- simplest case"
	"https://github.com/dalehenrich/Rowan/issues/393"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1  
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.

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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove method"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method1;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #().

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.