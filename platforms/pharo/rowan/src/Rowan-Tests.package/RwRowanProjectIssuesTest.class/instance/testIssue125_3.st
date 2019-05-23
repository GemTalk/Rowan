tests-issue 125
testIssue125_3

	"https://github.com/dalehenrich/Rowan/issues/125"

	"move a method from an extension method in one package ... unchanged to an extension method in another package"

	"RwGsPatchSetBasic does not understand:
		addPatchedExtendedClassMethodProperties:inClass:inPackage:inProject:
		addPatchedExtendedInstanceMethodProperties:inClass:inPackage:inProject:
	"
	| projectName packageName1 packageName2 packageName3 className theClass  |
	projectName := 'Issue125_Project'.
	packageName1 := 'Issue125-Core'.
	packageName2 := 'Issue125-Extensions1'.
	packageName3 := 'Issue125-Extensions2'.
	className := 'Issue125Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName3 toProjectNamed: projectName validate: false.

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

	theClass rwCompileMethod: 'foo ^1' category:'*' , packageName3 asLowercase.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName3 asLowercase).
