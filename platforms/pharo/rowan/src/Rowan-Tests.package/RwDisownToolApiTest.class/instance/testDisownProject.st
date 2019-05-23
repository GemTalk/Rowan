tests
testDisownProject

	| projectTools projectName packageNames className packageName1 packageName2 barMethod fooMethod 
		theClass x projectDefinitionToLoad projectDefinitionToAdopt |

	projectTools := Rowan projectTools.
	projectName := 'Disown Browser'.
	packageName1 := 'Disown-Core'.
	packageName2 := 'Disown-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
	className := 'DisownedClass'.

	projectDefinitionToLoad := self
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
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) notNil.
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	projectDefinitionToAdopt := (Rowan image loadedProjectNamed: projectName) asDefinition.
	self assert: (projectDefinitionToAdopt projectDefinitionSourceProperty = RwLoadedProject _projectLoadedDefinitionSourceValue).

	projectTools disown
		disownProjectNamed: projectName.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: (x := theClass rowanPackageName) = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: (x := barMethod rowanPackageName) = Rowan unpackagedName.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) isNil.
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) isNil.

	"reload original project definition --- project and packages only"
	projectTools load loadProjectDefinition: projectDefinitionToLoad.

	"adopt the complete project definition --- includes class, methods and extension methods"
	projectTools adopt
		adoptProjectDefinition: projectDefinitionToAdopt.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: (x := theClass rowanPackageName) = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) notNil.
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.