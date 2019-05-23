tests
testDisownPackage2

	| packageTools projectName packageNames className packageName1 packageName2 barMethod fooMethod theClass x |
	packageTools := Rowan packageTools.
	projectName := 'Disown Browser'.
	packageName1 := 'Disown-Core'.
	packageName2 := 'Disown-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
	className := 'DisownedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing package disown api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.
	barMethod := theClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName2 ifAbsent: []) notNil.
	self assert: (x := (Rowan image loadedClassExtensionsForClass: theClass) size) = 1.

	packageTools disown
		disownPackageNamed: packageName2.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = Rowan unpackagedName.
	self assert: (Rowan image loadedPackageNamed: packageName2 ifAbsent: []) isNil.
	self assert: (x := (Rowan image loadedClassExtensionsForClass: theClass) size) = 0.

	Rowan projectTools browser
		addPackageNamed: packageName2 toProjectNamed: projectName.
	packageTools adopt
		adoptClassExtensionNamed: className  instanceSelectors: #( #bar) classSelectors: #() intoPackageNamed: packageName2.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName2 ifAbsent: []) notNil.
