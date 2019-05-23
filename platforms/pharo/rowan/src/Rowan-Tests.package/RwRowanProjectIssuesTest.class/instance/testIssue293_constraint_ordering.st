tests-issue 293
testIssue293_constraint_ordering

	"https://github.com/dalehenrich/Rowan/issues/293"

	"constraints should be displayed in inst var order, not alphabetical order"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 projectSetDefinition class |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Subclass2'.
	className3 := 'Issue255Subclass3'.

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

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #( ivar2 ivar3 ivar4 ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar4' . 'Association' }. { 'ivar3' . 'Association' }. { 'ivar2' . 'Association' }. { 'ivar1' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #( ivar7 ivar6)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar7' . 'Association' }. { 'ivar6' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className2
		instvars: #( ivar8 ivar9)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar9' . 'Association' }. { 'ivar8' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association }.  { #'ivar6' . Association }. }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).
