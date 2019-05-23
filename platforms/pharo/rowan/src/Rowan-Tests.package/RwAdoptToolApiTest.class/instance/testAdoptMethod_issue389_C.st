tests
testAdoptMethod_issue389_C

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