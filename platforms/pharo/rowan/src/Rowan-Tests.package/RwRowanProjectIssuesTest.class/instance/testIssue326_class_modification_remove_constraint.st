tests-issue 326
testIssue326_class_modification_remove_constraint

	"test modifying constraints remove constraint"
	"https://github.com/dalehenrich/Rowan/issues/326"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass x |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project - class with constraints"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1 ivar2)
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.
	classDefinition1
		gs_constraints: { { 'ivar1' . 'Association' } . { 'ivar2' . 'Number' } }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1  ivar2).
	self assert: (x := class1 classVarNames asSortedCollection) = #() asSortedCollection.
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar2' . Number } . { #'ivar1' . Association } }).

"remove a constraint"
	classDefinition1
		gs_constraints: { { 'ivar2' . 'Number' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass == class1.
	self assert: class1 instVarNames = #(ivar1 ivar2).
	self assert: (class1 _constraintsEqual: { { #'ivar2' . Number } }).
