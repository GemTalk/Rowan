Class {
	#name : 'RwMoveTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_modified_class_structure_between_packages [

	"https://github.com/dalehenrich/Rowan/issues/286"

	"Class structure moved to a new package located in same symbol dictionary (precursor to #testProjectSet_move_modified_class_structure_between_packages_and_symbol_dicts)"

	"Old package structure:
		Issue_Command
			IssueSaveCommand
		IssueCommand
			IssueFileout
			IssueListClasses
	"

	"New packages structure:
		IssueCommand
			IssueFileout
			IssueListClasses
			IssueHelp
			IssueReport
	"


	| projectName  packageName1 packageName2  projectDefinition1 projectDefinition2 packageDefinition 
		className1 className2 className3 className4 className5 className6 className7
		projectSetDefinition class3 class4 class5  oldClass3 oldClass4 oldClass5 componentName |

	projectName := 'Issue'.
	packageName1 := 'Issue-Core1'.
	packageName2 := 'Issue-Core2'.
	className1 := 'Issue_Command'.
	className2 := 'IssueSaveCommand'.
	className3 := 'IssueCommand'.
	className4 := 'IssueFileout'.
	className5 := 'IssueListClasses'.
	className6 := 'IssueHelp'.
	className7 := 'IssueReport'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create old package structure"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.

	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className2
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.

"create new package structure"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectSetDefinition addDefinition: projectDefinition2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition
		removeClassNamed: className1;	"deleted"
		removeClassNamed: className2;	"deleted"
		removeClassNamed: className3;	"moved"
		removeClassNamed: className4;	"moved"
		removeClassNamed: className5;	"moved"
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className6
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className7
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	oldClass3 := class3.
	oldClass4 := class4.
	oldClass5 := class5.

	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.
	self assert: class3 == oldClass3.
	self assert: class4 == oldClass4.
	self assert: class5 == oldClass5.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_modified_class_structure_between_packages_and_symbol_dicts [

	"https://github.com/dalehenrich/Rowan/issues/286"

	"Class structure moved to a new package located in a different symbol dictionary"

	"Old package structure:
		Issue_Command
			IssueSaveCommand
		IssueCommand
			IssueFileout
			IssueListClasses
	"

	"New packages structure:
		IssueCommand
			IssueFileout
			IssueListClasses
			IssueHelp
			IssueReport
	"


	| projectName  packageName1 packageName2  projectDefinition1 projectDefinition2 packageDefinition 
		className1 className2 className3 className4 className5 className6 className7
		projectSetDefinition class3 class4 class5  oldClass3 oldClass4 oldClass5 project audit  
		componentName |

	projectName := 'Issue'.
	packageName1 := 'Issue-Core1'.
	packageName2 := 'Issue-Core2'.
	className1 := 'Issue_Command'.
	className2 := 'IssueSaveCommand'.
	className3 := 'IssueCommand'.
	className4 := 'IssueFileout'.
	className5 := 'IssueListClasses'.
	className6 := 'IssueHelp'.
	className7 := 'IssueReport'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create old package structure"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		resolve;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.

	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className2
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.

"create new package structure"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectSetDefinition addDefinition: projectDefinition2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition
		removeClassNamed: className1;	"deleted"
		removeClassNamed: className2;	"deleted"
		removeClassNamed: className3;	"moved"
		removeClassNamed: className4;	"moved"
		removeClassNamed: className5;	"moved"
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className6
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className7
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	self assert: (audit := project audit) isEmpty.
	oldClass3 := class3.
	oldClass4 := class4.
	oldClass5 := class5.

	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.
	self assert: class3 == oldClass3.
	self assert: class4 == oldClass4.
	self assert: class5 == oldClass5.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_modified_class_structure_with_new_class_version_between_packages_and_symbol_dicts_286 [

	"https://github.com/dalehenrich/Rowan/issues/286"

	"Class structure moved to a new package located in a different symbol dictionary. The surviving superclass has a new class version"

	"Old package structure:
		Issue_Command
			IssueSaveCommand
		IssueCommand
			IssueFileout
			IssueListClasses
	"

	"New packages structure:
		IssueCommand
			IssueFileout
			IssueListClasses
			IssueHelp
			IssueReport
	"


	| projectName  packageName1 packageName2  projectDefinition1 projectDefinition2 packageDefinition 
		className1 className2 className3 className4 className5 className6 className7
		projectSetDefinition class3 class4 class5  oldClass3 oldClass4 oldClass5 project audit
		componentName |

	projectName := 'Issue'.
	packageName1 := 'Issue-Core1'.
	packageName2 := 'Issue-Core2'.
	className1 := 'Issue_Command'.
	className2 := 'IssueSaveCommand'.
	className3 := 'IssueCommand'.
	className4 := 'IssueFileout'.
	className5 := 'IssueListClasses'.
	className6 := 'IssueHelp'.
	className7 := 'IssueReport'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create old package structure"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		resolve;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.

	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className2
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.

"create new package structure"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectSetDefinition addDefinition: projectDefinition2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition
		removeClassNamed: className1;	"deleted"
		removeClassNamed: className2;	"deleted"
		removeClassNamed: className3;	"moved"
		removeClassNamed: className4;	"moved"
		removeClassNamed: className5;	"moved"
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className6
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className7
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	self assert: (audit := project audit) isEmpty.
	oldClass3 := class3.
	oldClass4 := class4.
	oldClass5 := class5.

	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.
	self assert: class3 ~~ oldClass3.
	self assert: class4 ~~ oldClass4.
	self assert: class5 ~~ oldClass5.
]
