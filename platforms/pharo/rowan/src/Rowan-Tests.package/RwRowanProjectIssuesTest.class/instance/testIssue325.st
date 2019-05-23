tests-issue 325
testIssue325

	"https://github.com/dalehenrich/Rowan/issues/325"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 classDefinition packageDefinition 
		className1 className2 className3 projectSetDefinition theSymbolDict myUserProfile mySymbolList theSymDictIndex |

	projectName := 'Issue325'.
	packageName1 := 'Issue325-Core1'.
	packageName2 := 'Issue325-Core2'.
	packageName3 := 'Issue325-Core3'.
	className1 := 'Issue325Class1'. 
	className2 := 'Issue325Class2'. 
	className3 := 'Issue325Class3'. 

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create definitions"
	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName1 forPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName3;
		yourself.

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
	packageDefinition addClassDefinition: classDefinition.

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
	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName3
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition := projectDefinition1 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"test"
	theSymbolDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: theSymbolDict class == SymbolDictionary.			"symbol dictionary is accessible in my symbol list"

	self assert: (Rowan image packageNames includes: packageName1).	"package 1 is visible"
	self assert: (Rowan globalNamed: className1) notNil.							"and class1 is visible"
	self assert: (Rowan image packageNames includes: packageName2).	"package 2 is visible"
	self assert: (Rowan globalNamed: className2) notNil.							"and class1 3 is visible"
	self assert: (Rowan image packageNames includes: packageName3).	"package is visible"
	self assert: (Rowan globalNamed: className3) notNil.							"and class1 is visible"

	myUserProfile := System myUserProfile.
	mySymbolList := System myUserProfile symbolList.
	theSymDictIndex := mySymbolList indexOf: theSymbolDict.
	myUserProfile removeDictionaryAt: theSymDictIndex.				"remove symbol dictionary from my symbol list"

	self deny: (Rowan image packageNames includes: packageName1).	"package 1 is visible"
	self deny: (Rowan globalNamed: className1) notNil.							"and class1 is visible"
	self assert: (Rowan image packageNames includes: packageName2).	"package 2 is visible"
	self assert: (Rowan globalNamed: className2) notNil.							"and class1 3 is visible"
	self assert: (Rowan image packageNames includes: packageName3).	"package is visible"
	self assert: (Rowan globalNamed: className3) notNil.							"and class1 is visible"

	self assert: (Rowan image packageNamesForLoadedProjectNamed: projectName) sort = {packageName2. packageName3} sort
