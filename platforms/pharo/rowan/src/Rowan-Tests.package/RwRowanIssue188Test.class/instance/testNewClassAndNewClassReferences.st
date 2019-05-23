tests
testNewClassAndNewClassReferences

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

	| projectName packageName className1 className2 className3 theClass1 theClass2 theClass3 oldTheClass1 oldTheClass3 |
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

	self assert: theClass2 new w == oldTheClass1.

	"Setup new class and new class reference:"
	self 
		_createNewClassAndReferenceMethodsInSingleLoad: projectName 
		packageName: packageName 
		classNames: {className1. className2. className3}.

	oldTheClass3 := theClass3 := Rowan globalNamed: className3.

	self assert: theClass2 new w == oldTheClass1.
	self assert: theClass2 new x == oldTheClass3.

	"Failure: the methods compiled with references to C as a newly created class are incorrect"
	theClass3 := Object
		rwSubclass: className3
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: theClass3 ~~ oldTheClass3.
	self assert: (theClass3 classHistory includes: oldTheClass3).
	self assert: theClass2 new w == theClass1.		"association in method #w is still correct"
	self assert: theClass2 new x == theClass3.		"association in method #x is correct"
