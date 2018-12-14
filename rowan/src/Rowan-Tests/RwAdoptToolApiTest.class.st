Class {
	#name : 'RwAdoptToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwAdoptToolApiTest >> _validateExpectedMonticelloConventionFailure_389_A: audit packageNames: packageNames className: className [

	"validate expected audit failures - according to Monticello conventions"

	| unexpectedFailures |
	self _validateExpectedMonticelloConventionFailure_389: audit packageName: (packageNames at: 2) className: className.
	audit removeKey: (packageNames at: 2).

	"class category not following Monticello conventions"
	unexpectedFailures := ((audit at: (packageNames at: 1)) at: className)
		reject: [:each | (each value = 'Class category has changed in compiled class v loaded class')
			or: [ (each value = 'Missing instance method extension category ') or: [ (each value = 'Missing class method extension category ')
			or: [ (each value = 'Missing loaded instance method. ') or: [ (each value = 'Missing loaded classmethod ') ]] ] ] ].
	self assert: unexpectedFailures isEmpty
]

{ #category : 'private' }
RwAdoptToolApiTest >> _validateExpectedMonticelloConventionFailure_389_B: audit packageName: packageName className: className [

	"validate expected audit failures - according to Monticello conventions"

	self _validateExpectedMonticelloConventionFailure_389: audit packageName: packageName className: className.
	audit removeKey: packageName.
	self assert: audit isEmpty
]

{ #category : 'private' }
RwAdoptToolApiTest >> _validateExpectedMonticelloConventionFailure_389: audit packageName: packageName className: className [

	"validate expected audit failures - according to Monticello conventions"

	| failures unexpectedFailures |
	failures := audit at: packageName.
	self assert: failures size = 1.
	failures := failures at: className.
	self assert: failures size = 2.

	"extension categories not named according to Monticello conventions (no leading $*)"
	unexpectedFailures := failures reject: [:each | (each value = 'Missing instance method extension category ') or: [ each value = 'Missing class method extension category ' ] ].
	self assert: unexpectedFailures isEmpty
]

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
RwAdoptToolApiTest >> testAdoptMethod_issue389_1 [

	"https://github.com/GemTalk/Rowan/issues/389"

	"reproduce original bug using adoptMethod:protocol:inClassNamed:isMeta:intoPackageNamed:"

	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1. packageName2 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create class and method"
	theClass := Object
		rwSubclass:className
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

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.


"change method using non-Rowan api"
	instanceMethod := theClass
		compileMethod: 'foo ^"comment" ''foo'''
		dictionaries: Rowan image symbolList
		category: 'accessing'
		environmentId: 0.

"confirm conruption"
	self assert: instanceMethod rowanPackageName =  Rowan unpackagedName.

"repair corruption using adopt"
	Rowan packageTools adopt
		adoptMethod: #foo
		protocol: 'accessing'
		inClassNamed: className
		isMeta: false
		intoPackageNamed:  packageName1.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty. " with bug: 'AdoptedClass #*adopt-core'->'Missing instance method extension category '"
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_2 [

	"https://github.com/GemTalk/Rowan/issues/389"

	"reproduce original bug using adoptMethod:inClassNamed:isMeta:intoPackageNamed:"

	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1. packageName2 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create class and method"
	theClass := Object
		rwSubclass:className
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

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.


"change method using non-Rowan api"
	instanceMethod := theClass
		compileMethod: 'foo ^"comment" ''foo'''
		dictionaries: Rowan image symbolList
		category: 'accessing'
		environmentId: 0.

"confirm conruption"
	self assert: instanceMethod rowanPackageName =  Rowan unpackagedName.

"repair corruption using adopt"
	Rowan packageTools adopt
		adoptMethod: #foo
		inClassNamed: className
		isMeta: false
		intoPackageNamed:  packageName1.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty. " with bug: 'AdoptedClass #*adopt-core'->'Missing instance method extension category '"
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_A [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into a different package than the packaged class; 
		don't use Monticello convention.
	 methods should be extension methods."

	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1. packageName2 }.
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

	self assert: theClass rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptClassNamed: className  
			intoPackageNamed: packageName1.

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

	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName2;
		adoptMethod: #bar 
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName2;
		yourself.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName2.
	self assert: classMethod rowanPackageName = packageName2.

"audit --> category not correct according to Monticello conventions"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

	self _validateExpectedMonticelloConventionFailure_389_A: audit packageNames: packageNames className: className
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_B [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into an unpackaged class; 
		don't use Monticello convention.
	 methods should be extension methods."

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Extensions'.
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
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName;
		adoptMethod: #bar 
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName;
		yourself.

"validate"
	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

"audit --> category not correct according to Monticello conventions"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

	self _validateExpectedMonticelloConventionFailure_389_B: audit packageName: packageName className: className.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_C [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into a different package than the packaged class; 
		use Monticello convention (_A).
	 methods should be extension methods."

	"adopt methods into a packaged class change protocol to use Monticello convention (_A)"

	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1. packageName2 }.
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
	theClass category: packageName1.

	self assert: theClass rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptClassNamed: className  
			intoPackageNamed: packageName1.

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

	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			protocol: '*', packageName2 asLowercase
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName2;
		adoptMethod: #bar 
			protocol: '*', packageName2 asLowercase
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName2;
		yourself.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName2.
	self assert: classMethod rowanPackageName = packageName2.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_D [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into an unpackaged class; 
		don't use Monticello convention (_B).
	 methods should be extension methods."

	"adopt methods change protocol to use Monticello convention (_B)"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Extension'.
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
			protocol: 'accessing'
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName;
		adoptMethod: #bar 
			protocol: 'accessing'
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName;
		yourself.

"validate"
	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

"audit --> category not correct according to Monticello conventions"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

	self _validateExpectedMonticelloConventionFailure_389_B: audit packageName: packageName className: className
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
