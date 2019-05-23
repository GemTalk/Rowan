tests
testOriginalFailureMode

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

	self assert: theClass2 new w == oldTheClass1.

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
	self assert: theClass2 new w == theClass1.

	"Original Setup: the method #w is compiled with a reference to className1, but it is in an incorrect association -- not yet apparent"
	theClass2 := Object
		rwSubclass: className2
		instVarNames: #(xxx)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: theClass2 new w == theClass1.

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
	self assert: theClass2 new w == theClass1.		"association in method #w incorrect"
