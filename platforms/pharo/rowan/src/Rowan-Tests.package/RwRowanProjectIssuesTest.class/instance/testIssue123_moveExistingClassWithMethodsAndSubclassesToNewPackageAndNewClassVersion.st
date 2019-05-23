tests-issue 123
testIssue123_moveExistingClassWithMethodsAndSubclassesToNewPackageAndNewClassVersion
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className1 className2 theClass1  theClass2 x oldClass |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className1 := 'Issue123Class'.
	className2 := 'Issue123SubClass'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass2 := (Rowan globalNamed: className1)
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName1.
	self assert: theClass1 rowanPackageName = packageName1.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.

	oldClass := theClass1.
	theClass1 := Object
		rwSubclass: className1
		instVarNames: #( ivar1 )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: oldClass ~~ theClass1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName2.
	self assert: theClass1 rowanPackageName = packageName2.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName2.
	self assert: (x := Rowan globalNamed: className2) ~~ theClass2.
	theClass2 := Rowan globalNamed: className2.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1
