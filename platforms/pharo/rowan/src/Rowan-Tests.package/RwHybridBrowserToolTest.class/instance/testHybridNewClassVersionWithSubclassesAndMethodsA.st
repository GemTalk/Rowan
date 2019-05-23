tests
testHybridNewClassVersionWithSubclassesAndMethodsA

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName1 asLowercase.
	class class
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	subclass
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.
	subclass class
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.

	self assert: subclass superClass == class.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: newSubclassVersion foo = 'foo'.
	self assert: newSubclassVersion foo1 = 'foo'.
	self assert: newSubclassVersion new foo = 'foo'.
	self assert: newSubclassVersion new foo1 = 'foo'