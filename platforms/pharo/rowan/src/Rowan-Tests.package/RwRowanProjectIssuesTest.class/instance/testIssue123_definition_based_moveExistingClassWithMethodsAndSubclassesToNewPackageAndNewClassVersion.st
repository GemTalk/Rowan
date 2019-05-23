tests-issue 123
testIssue123_definition_based_moveExistingClassWithMethodsAndSubclassesToNewPackageAndNewClassVersion
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className1 className2 projectDefinition classDefinition1 classDefinition2 packageDefinition
		projectSetDefinition theClass1 theClass2 ivNames oldClass1 oldClass2 |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className1 := 'Issue123Class'.
	className2 := 'Issue123SubClass'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'foo ^1').
	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2.

"load project1"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName1.
	self assert: theClass1 rowanPackageName = packageName1.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'accessing'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.

"create new version of project"
	classDefinition1 := packageDefinition classDefinitions at: className1.
	packageDefinition removeClassDefinition: classDefinition1.

	ivNames := classDefinition1 instVarNames copy.
	ivNames add: 'ivar1'.
	classDefinition1 instVarNames: ivNames.
	classDefinition1 category: packageName2.

	packageDefinition := projectDefinition packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition1.

"load project1"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	oldClass1 := theClass1.
	oldClass2 := theClass2.
	theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.
	self assert: oldClass1 ~~ theClass1.
	self assert: oldClass2 ~~ theClass2.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName2.
	self assert: theClass1 rowanPackageName = packageName2.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'accessing'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName2.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.
