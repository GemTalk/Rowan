tests-issue 275
testIssue275_delete_subclass_handle_keep_in_system

	"https://github.com/dalehenrich/Rowan/issues/275"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 packageDefinition className1 className2 
		projectSetDefinition class1 class2 oldClass1 oldClass2 handled |

	projectName := 'Issue275'.
	packageName := 'Issue275-Core'.
	className1 := 'Issue275Class1'.
	className2 := 'Issue275Subclass1'.

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

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 isBehavior.
	self assert: class1 name asString = className1.
	self assert: class2 isBehavior.
	self assert: class2 name asString = className2.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.

"remove class2 from system new version of class1"
	classDefinition1 instVarNames: #(ivar1).
	packageDefinition
		removeClassDefinition: classDefinition2.

"load -- remove from loaded things, but leave in system"
	handled := false.
	[ Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwDeleteClassFromSystemNotification
		do: [:ex | 
			ex candidateClass name asString = className2 
				ifTrue: [ 
					handled := true.
					ex resume: false ]
				ifFalse: [ ex pass ] ].
	self assert: handled.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 ~~ oldClass1.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 notNil.
	self assert: class2 superclass == oldClass1.
	self assert: class2 == oldClass2.
	self assert: class2 rowanPackageName = Rowan unpackagedName.
