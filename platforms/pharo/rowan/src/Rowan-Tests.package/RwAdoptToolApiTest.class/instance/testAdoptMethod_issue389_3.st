tests
testAdoptMethod_issue389_3

	"https://github.com/GemTalk/Rowan/issues/389"

	"use adoptMethod:protocol:inClassNamed:isMeta:intoPackageNamed: where protocol is using Monticello convention"

	| projectName packageNames className packageName1 theClass symDict symDictName instanceMethod classMethod audit expectedFailure |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageNames := {packageName1 }.
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
		protocol: '*', packageName1 asLowercase "will cause audit to fail"
		inClassNamed: className
		isMeta: false
		intoPackageNamed:  packageName1.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty. "expect: 'AdoptedClass #*adopt-core'->'Extension category name can not be same as class package'"

	self assert: audit size = 1.
	expectedFailure := ((audit at: packageName1) at: className).
	self assert: expectedFailure size = 1.
	self assert: expectedFailure first key =  'AdoptedClass #*adopt-core'.
	self assert: expectedFailure first value = 'Extension category name can not be same as class package'.