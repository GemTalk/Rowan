tests-issue 91
testIssue91
	"add class in one package, add extension in another package, new version of class, 
		update extension method - boom?"

	"https://github.com/dalehenrich/Rowan/issues/91"

	| projectName packageNames packageName1 packageName2 className testClass testInstance newClass fooMethod |
	projectName := 'Issue91'.
	packageName1 := 'Issue91-Core'.
	packageName2 := 'Issue91-Extensions'.
	packageNames := {packageName1. packageName2}.
	className := 'Issue91Class'.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This project is being used to reproduce the problem reported in Issue #91.'
		className: className
		inPackageName: packageName1.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	testClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase. "create extension method"

	newClass := Object rwSubclass: 'Issue91Class'
		instVarNames: #( ivar1 ivar2)
		classVars: #( Cvar1)
		classInstVars: #( civar1)
		poolDictionaries: #()
		category: 'Simple Things'
		packageName: 'Issue91-Core'
		constraints: #()
		options: #().

	self assert: newClass ~~ testClass.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.
	self assert: testInstance foo = 'foo'.

	fooMethod := testClass
		rwCompileMethod: 'foo ^''bar'''
		category: '*' , packageName2 asLowercase. "create extension method"

	self assert: testInstance foo = 'bar'.
	self assert: fooMethod rowanPackageName = packageName2.
