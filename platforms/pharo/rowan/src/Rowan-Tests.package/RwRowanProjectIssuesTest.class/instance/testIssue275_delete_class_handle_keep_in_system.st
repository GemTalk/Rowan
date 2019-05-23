tests-issue 275
testIssue275_delete_class_handle_keep_in_system

	"https://github.com/dalehenrich/Rowan/issues/275"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass |

	projectName := 'Issue275'.
	packageName := 'Issue275-Core'.
	className1 := 'Issue275Class1'.

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
		instvars: #()
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
	self assert: class1 isBehavior.
	self assert: class1 name asString = className1.

"remove class from system"
	packageDefinition
		removeClassDefinition: classDefinition1.

"load -- remove from loaded things, but leave in system"
	[ Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwDeleteClassFromSystemNotification
		do: [:ex | 
			ex candidateClass == class1 
				ifTrue: [ ex resume: false ]
				ifFalse: [ ex pass ] ].

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: class1 == oldClass.
	self assert: class1 rowanProjectName = Rowan unpackagedName.
