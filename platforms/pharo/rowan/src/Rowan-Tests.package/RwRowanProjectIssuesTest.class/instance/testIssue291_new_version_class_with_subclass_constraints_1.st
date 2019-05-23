tests-issue 291
testIssue291_new_version_class_with_subclass_constraints_1

	"https://github.com/dalehenrich/Rowan/issues/291"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 |

	projectName := 'Issue291'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Class3'.

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

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' }. };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Association } }).
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).



"modify class -- new version"
	classDefinition1
		instVarNames: #(ivar5 ivar1);
		gs_constraints: { { 'ivar1' . 'Association' } . { 'ivar5' . 'Number' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass1 ~~ class1.
	self assert: class1 instVarNames = #(ivar5 ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar5' . Number }. { #'ivar1' . Association }. }).
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	self assert: oldClass2 ~~ class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: oldClass3 ~~ class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).
