tests-issue 123
testIssue123_moveExistingClassWithExtensionMethodsToNewPackage
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add extension method category, *rowansample1-extensions, in class Issue91
    Add method extendedMethod in new category.
    Verify by Show Changes that RowanSample1-Extensions package shows the new method was added.
    Using the class definition template, move the class to RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className theClass  |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className := 'Issue123Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName1.
	self assert: theClass rowanPackageName = packageName1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.

	theClass := Object
		rwSubclass: className
		instVarNames: #( )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: theClass category = packageName2.
	self assert: theClass rowanPackageName = packageName2.
	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.
