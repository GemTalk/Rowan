tests-issue 255
testIssue255_class_modification_no_constraints

	"test modifying constraints"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass x |

	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.

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
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: (x := class1 classVarNames asSortedCollection) = #() asSortedCollection.
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.

"modify class -- no new version"
	classDefinition1
		classVarNames: #(Cvar1 Cvar2 Cvar3);
		category: 'newer category';
		comment: 'newer comment'.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass == class1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: (x := class1 classVarNames asSortedCollection) = #(Cvar1 Cvar2 Cvar3) asSortedCollection.
	self assert: class1 category = 'newer category'.
	self assert: class1 comment = 'newer comment'.

"modify class -- new version"
	classDefinition1
		instVarNames: #(ivar1 ivar2);
		classVarNames: #(Cvar1 Cvar2 Cvar3);
		category: 'new category';
		comment: 'new comment'.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass ~~ class1.
	self assert: class1 instVarNames = #(ivar1 ivar2).
	self assert: (x := class1 classVarNames asSortedCollection) = #(Cvar1 Cvar2 Cvar3) asSortedCollection.
	self assert: class1 category = 'new category'.
	self assert: class1 comment = 'new comment'.
