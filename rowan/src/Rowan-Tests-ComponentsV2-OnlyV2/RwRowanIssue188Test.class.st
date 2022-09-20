Class {
	#name : 'RwRowanIssue188Test',
	#superclass : 'RwRowanProjectIssuesTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'Testing' }
RwRowanIssue188Test class >> shouldInheritSelectors [

	^false

]

{ #category : 'private' }
RwRowanIssue188Test >> _createBaselineClassesInSingleLoad: projectName packageName: packageName classNames: classNames [

	"
		Set up baseline:
			1. Class A and B
			2. Class B has a method w that returns class A.
	"

	| projectDefinition packageDefinition componentName |
	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName  }
			toComponentNamed: componentName;
		resolveProject;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classNames do: [:className |
		| classDefinition |
		classDefinition := RwClassDefinition
			newForClassNamed: className
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName
			comment: 'I am a ', className, ' class'
			pools: #()
			type: 'normal'.
		packageDefinition addClassDefinition: classDefinition ].

	(packageDefinition classDefinitions at: (classNames at: 2))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'w'
						protocol: 'accessing'
						source: 'w ^ ', (classNames at: 1)).

	projectDefinition load
]

{ #category : 'private' }
RwRowanIssue188Test >> _createNewClassAndReferenceMethodsInSingleLoad: projectName packageName: packageName classNames: classNames [

	"
	Setup new class and new class reference:
		In a single load:
			1. Create new class C
			2. add method x in B to return class C
	"

	| projectDefinition packageDefinition classDefinition |

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames at: 3)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition addClassDefinition: classDefinition.

	(packageDefinition classDefinitions at: (classNames at: 2))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'x'
						protocol: 'accessing'
						source: 'x ^ ', (classNames at: 3)).

	projectDefinition load
]

{ #category : 'tests' }
RwRowanIssue188Test >> testBaselineScenario [

	"https://github.com/dalehenrich/Rowan/issues/188"

	"
		Set up baseline:
			1. Class A and B
			2. Class B has a method w that returns class A.
		In a single load:
			1. Create new version of class A
		Verify method w in class B returns the new version of class A
	"

	| projectName packageName className1 className2  theClass1 theClass2 oldTheClass1 |
	projectName := 'Issue_188'.
	packageName := 'Issue_188-Core'.
	className1 := 'Issue_188_A'.
	className2 := 'Issue_188_B'.

	self _createBaselineClassesInSingleLoad: projectName packageName: packageName classNames: {className1. className2}.

	oldTheClass1 := theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.

	self assert: (theClass2 new perform: #w) == oldTheClass1.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: theClass1 ~~ oldTheClass1.
	self assert: (theClass1 classHistory includes: oldTheClass1).
	self assert: (theClass2 new perform: #w) == theClass1.
]

{ #category : 'tests' }
RwRowanIssue188Test >> testNewClassAndNewClassReferences [

	"https://github.com/dalehenrich/Rowan/issues/188"

	"newly created classes end up adding the (new) association in tempSymbols to the target symbol dictionary,
		so they are not exposed to same bug"

	"
	Baseline:
		Set up baseline:
			1. Class A and B
			2. Class B has a method w that returns class A.
		Verify method w in class B returns the new version of class A
	Setup new class and new class reference:
		In a single load:
			1. Create new class C
			2. add method x in B to return class C
	Failiure:
			1. Create a new version of class A
		Verify that when method x is invoked that the new method w is called
			and verify that method z returns the current version of C
	"

	| projectName packageName className1 className2 className3 theClass1 theClass2 theClass3 oldTheClass1 oldTheClass3 audit |
	projectName := 'Issue_188'.
	packageName := 'Issue_188-Core'.
	className1 := 'Issue_188_A'.
	className2 := 'Issue_188_B'.
	className3 := 'Issue_188_C'.

	"Baseline"
	self 
		_createBaselineClassesInSingleLoad: projectName 
		packageName: packageName 
		classNames: {className1. className2}.

	oldTheClass1 := theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.

	self assert: (theClass2 new perform: #w) == oldTheClass1.

	"Setup new class and new class reference:"
	self 
		_createNewClassAndReferenceMethodsInSingleLoad: projectName 
		packageName: packageName 
		classNames: {className1. className2. className3}.

	oldTheClass3 := theClass3 := Rowan globalNamed: className3.

	self assert: (theClass2 new perform: #w) == oldTheClass1.
	self assert: (theClass2 new perform: #x) == oldTheClass3.

	"Failure: the methods compiled with references to C as a newly created class are incorrect"
	theClass3 := Object
		rwSubclass: className3
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.
	self assert: theClass3 ~~ oldTheClass3.
	self assert: (theClass3 classHistory includes: oldTheClass3).
	self assert: (theClass2 new perform: #w) == theClass1.		"association in method #w is still correct"
	self assert: (theClass2 new perform: #x) == theClass3.		"association in method #x is correct"
]

{ #category : 'tests' }
RwRowanIssue188Test >> testOriginalFailureMode [

	"https://github.com/dalehenrich/Rowan/issues/188"

	"
	Baseline:
		Set up baseline:
			1. Class A and B
			2. Class B has a method w that returns class A.
		In a single load:
			1. Create new version of class A
		Verify method w in class B returns the new version of class A
	Original Setup:
		In a single load:
			1. create new version of B
	Original Failure:
		In a single load:
			1. Create new version of class A
		Verify method w in class B returns the correct version of class A
	"

	| projectName packageName className1 className2  theClass1 theClass2 oldTheClass1 |
	projectName := 'Issue_188'.
	packageName := 'Issue_188-Core'.
	className1 := 'Issue_188_A'.
	className2 := 'Issue_188_B'.

	"Baseline"
	self _createBaselineClassesInSingleLoad: projectName packageName: packageName classNames: {className1. className2}.

	oldTheClass1 := theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.

	self assert: (theClass2 new perform: #w) == oldTheClass1.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: theClass1 ~~ oldTheClass1.
	self assert: (theClass1 classHistory includes: oldTheClass1).
	self assert: (theClass2 new perform: #w) == theClass1.

	"Original Setup: the method #w is compiled with a reference to className1, but it is in an incorrect association -- not yet apparent"
	theClass2 := Object
		rwSubclass: className2
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: (theClass2 new perform: #w) == theClass1.

	"Original Failure: the incorrect association in method #w is not updated with the new version of className1 - boom"
	oldTheClass1 := theClass1.
	theClass1 := Object
		rwSubclass: className1
		instVarNames: #(xxx yyy)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: theClass1 ~~ oldTheClass1.
	self assert: (theClass1 classHistory includes: oldTheClass1).
	self assert: (theClass2 new perform: #w) == theClass1.		"association in method #w incorrect"
]

{ #category : 'tests' }
RwRowanIssue188Test >> testOriginalSetupMode [

	"https://github.com/dalehenrich/Rowan/issues/188"

	"
	Baseline:
		Set up baseline:
			1. Class A and B
			2. Class B has a method w that returns class A.
		In a single load:
			1. Create new version of class A
		Verify method w in class B returns the new version of class A
	Original Setup:
		In a single load:
			1. create new version of B
		Verify method w in class B returns the correct version of class A
	"

	| projectName packageName className1 className2  theClass1 theClass2 oldTheClass1 |
	projectName := 'Issue_188'.
	packageName := 'Issue_188-Core'.
	className1 := 'Issue_188_A'.
	className2 := 'Issue_188_B'.

	"Baseline"
	self _createBaselineClassesInSingleLoad: projectName packageName: packageName classNames: {className1. className2}.

	oldTheClass1 := theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.

	self assert: (theClass2 new perform: #w) == oldTheClass1.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: theClass1 ~~ oldTheClass1.
	self assert: (theClass1 classHistory includes: oldTheClass1).
	self assert: (theClass2 new perform: #w) == theClass1.

	"Original Setup: the method #w is compiled with a reference to className1, but it is in an incorrect association -- not yet apparent"
	theClass2 := Object
		rwSubclass: className2
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: (theClass2 new perform: #w) == theClass1.
]
