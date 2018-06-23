Class {
	#name : 'RwDisownToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownClass1 [

	"disown a class with a method"

	| packageTools projectName packageNames className packageName theClass fooMethod |
	packageTools := Rowan packageTools.
	projectName := 'DisProject'.
	packageName := 'Disown-Core'.
	packageNames := {packageName}.
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
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName asLowercase.

	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = packageName.
	self assert: fooMethod rowanPackageName = packageName.

	packageTools disown
		disownClassNamed: className.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.

	packageTools adopt
		adoptClassNamed: className  intoPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = packageName.
	self assert: fooMethod rowanPackageName = packageName.

]

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownClass2 [

	"disown a class with a method and an extenstion method"

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

	packageTools disown
		disownClassNamed: className.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: (x := barMethod rowanPackageName) = packageName2.

	packageTools adopt
		adoptClassNamed: className  intoPackageNamed: packageName1.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownExtensionMethods [

	"disown extension methods in a class"

	| packageTools projectName packageNames className packageName1 packageName2 barMethod fooMethod theClass |
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

	packageTools disown
		disownClassExtensionMethodsInClassNamed: className forPackageNamed: packageName2.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = Rowan unpackagedName.

	packageTools adopt
		adoptClassExtensionNamed: className  instanceSelectors: #( #bar) classSelectors: #() intoPackageNamed: packageName2.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownMethod [
	| packageTools projectName packageNames className packageName theClass fooMethod package |
	packageTools := Rowan packageTools.
	projectName := 'DisProject'.
	packageName := 'Disown-Core'.
	packageNames := {packageName}.
	className := 'DisownedClass'.
	package := RwPackage newNamed: packageName.

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
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName asLowercase.

	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = packageName.
	self assert: fooMethod rowanPackageName = packageName.

	packageTools disown
		disownMethod: #foo inClassNamed: className isMeta: false.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = packageName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.


	packageTools adopt
		adoptMethod: #foo inClassNamed: className isMeta: false  intoPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: fooMethod rowanPackageName = packageName.

]

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownPackage1 [

	"disown a class with a method and an extenstion method"

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
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) notNil.

	packageTools disown
		disownPackageNamed: packageName1.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: (x := barMethod rowanPackageName) = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) isNil.

	Rowan projectTools browser
		addPackageNamed: packageName1 toProjectNamed: projectName.
	packageTools adopt
		adoptClassNamed: className  intoPackageNamed: packageName1.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) notNil.

]

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownPackage2 [

	"disown extension methods in a class"

	| packageTools projectName packageNames className packageName1 packageName2 barMethod fooMethod theClass |
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

]
