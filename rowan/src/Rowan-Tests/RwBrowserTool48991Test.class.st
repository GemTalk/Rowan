Class {
	#name : 'RwBrowserTool48991Test',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwBrowserTool48991Test >> testAddMethod [
	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool compiledMethod gsNMethod project audit |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageName3 := 'Simple-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	browserTool := RwPrjBrowserTool new.

	"add new method"
	gsNMethod := browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #'ivar1:' with: 3.
	self assert: (testInstance perform: #'ivar1') = 3.
	self should: [ testInstance perform: #'foo' ] raise: MessageNotUnderstood.
	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'ivar1:'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName1.

	"add new extension method"
	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: '*' , packageName2 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.

	self assert: (audit := project audit) isEmpty.
	self assert: (testInstance perform: #'foo') = 'bar'.

	gsNMethod := browserTool
		addOrUpdateMethod: 'foo ^''baz'''
		inProtocol: '*' , packageName2 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	self assert: (audit := project audit) isEmpty.
	self assert: (testInstance perform: #'foo') = 'baz'.

	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'foo'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName2
]
