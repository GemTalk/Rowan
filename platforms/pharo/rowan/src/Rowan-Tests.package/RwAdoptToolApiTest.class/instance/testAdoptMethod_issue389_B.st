tests
testAdoptMethod_issue389_B

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