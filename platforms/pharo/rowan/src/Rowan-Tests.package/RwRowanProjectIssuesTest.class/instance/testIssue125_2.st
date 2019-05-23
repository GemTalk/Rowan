tests-issue 125
testIssue125_2

	"https://github.com/dalehenrich/Rowan/issues/125"

	"move a method from an extension method ... unchanged"

	"RwGsPatchSetBasic does not understand:
		addPatchedExtendedClassMethodProperties:inClass:inPackage:inProject:
		addPatchedExtendedInstanceMethodProperties:inClass:inPackage:inProject:
	"
	| projectName packageName1 packageName2 className theClass  x |
	projectName := 'Issue125_Project'.
	packageName1 := 'Issue125-Core'.
	packageName2 := 'Issue125-Extensions'.
	className := 'Issue125Class'.

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
	theClass rwCompileMethod: 'foo ^1' category: '*' , packageName2 asLowercase.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).

	theClass rwCompileMethod: 'foo ^1' category: 'accessing'.

	self assert: theClass new foo = 1.
	self assert: (x := (theClass categoryOfSelector: #foo) asString) = 'accessing'
