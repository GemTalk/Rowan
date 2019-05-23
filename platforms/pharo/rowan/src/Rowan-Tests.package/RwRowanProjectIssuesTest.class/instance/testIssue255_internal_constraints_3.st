tests-issue 255
testIssue255_internal_constraints_3

	"reproduce bug encountered at client site - constraints on subclasses"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 projectSetDefinition 
		class1 class2 className3 |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Array3'.

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

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . className2 } }.

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #(ivar2)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition2 gs_constraints: { { 'ivar2' . className3 } }.

	classDefinition3 := RwClassDefinition
		newForClassNamed: className3
		super: 'Array'
		instvars: #()
		classinstvars: #( )
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition3 gs_constraints: { className2 }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2;
		addClassDefinition: classDefinition3.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.
