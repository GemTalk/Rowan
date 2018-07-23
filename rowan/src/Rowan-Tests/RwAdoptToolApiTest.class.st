Class {
	#name : 'RwAdoptToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptClass [
	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptClassNamed: className 
		intoPackageNamed: packageName.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptClassWithExistingExtensionMethods_1 [
	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName fooMethod barMethod symbolList |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core1'.
	packageName2 := 'Adopt-Core2'.
	packageNames := {packageName1. packageName2}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	fooMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	barMethod := theClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: barMethod rowanPackageName = packageName2.

	Rowan packageTools adopt 
		adoptClassNamed: className 
		intoPackageNamed: packageName1.

	self assert: theClass rowanProjectName = projectName.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod [
	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			protocol: '*', packageName 
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName;
		adoptMethod: #bar 
			protocol: '*', packageName 
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName;
		yourself.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolDictionary_1 [

	"simplest case"

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptSymbolDictionaryNamed: symDictName 
		intoPackageNamed: packageName.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolDictionary_2 [

	"packaged and unpackaged code sharing a symbol dictionary"

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className1 className2 packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core1'.
	packageName2 := 'Adopt-Core2'.
	packageNames := {packageName1. packageName2.}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass1'.
	className2 := 'AdoptedClass2'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create package class and methods"

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	instanceMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	classMethod := theClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

	"Use non-Rowan api to create unpackaged class and methods"
	theClass := Object subclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptSymbolDictionaryNamed: symDictName 
		intoPackageNamed: packageName2.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName2.
	self assert: instanceMethod rowanPackageName = packageName2.
	self assert: classMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolList_1 [

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList packageNameMap
		excludedSymbolDictionaries |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	packageNameMap := Dictionary new
		at: symDictName asString put: packageName;
		yourself.
	excludedSymbolDictionaries := symbolList reject: [:symbolDictionary | symbolDictionary name = symDictName ].
	Rowan packageTools adopt 
		adoptSymbolList: symbolList 
		excluding: excludedSymbolDictionaries 
		intoPackagesNamed: packageNameMap.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolList_2 [

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList packageNameMap
		excludedSymbolDictionaryNames |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	packageNameMap := Dictionary new
		at: symDictName asString put: packageName;
		yourself.
	excludedSymbolDictionaryNames := (symbolList reject: [:symbolDictionary | symbolDictionary name = symDictName ]) collect: [:each | each name asString ].
	Rowan packageTools adopt 
		adoptSymbolList: symbolList 
		excludingSymbolDictsNamed: excludedSymbolDictionaryNames 
		intoPackagesNamed: packageNameMap.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.

]
