tests-issue 467
testIssue467_new_version_class_with_subclasses_4

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 extraClassDefinition audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

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
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_1'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_2'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_3'
			super: className2, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_1'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_2'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_3'
			super:  className3, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.