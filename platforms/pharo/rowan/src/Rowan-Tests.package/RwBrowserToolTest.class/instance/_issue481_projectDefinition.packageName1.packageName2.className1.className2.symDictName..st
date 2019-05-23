private
_issue481_projectDefinition: projectName  packageName1: packageName1 packageName2: packageName2 className1: className1 className2: className2 symDictName: symDictName

	| theClass1 theClass2 symDict instanceMethod1 classMethod1 symbolList 
		instanceMethod2 classMethod2 |
	symbolList := Rowan image symbolList.

	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass1 := Object subclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass1 category: packageName1.

	instanceMethod1 := theClass1
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	classMethod1 := theClass1 class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	Rowan packageTools adopt 
		adoptClassNamed: className1
		intoPackageNamed: packageName1.

	self assert: theClass1 rowanPackageName = packageName1.
	self assert: instanceMethod1 rowanPackageName = packageName1.
	self assert: classMethod1 rowanPackageName = packageName1.

	theClass2 := Object subclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass2 category: packageName2.

	instanceMethod2 := theClass2
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: '*', packageName2 asLowercase
		environmentId: 0.

	classMethod2 := theClass2 class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: '*', packageName2 asLowercase
		environmentId: 0.

	Rowan packageTools adopt 
		adoptClassExtensionNamed: className2  
		instanceSelectors: #(foo) 
		classSelectors: #(bar)
		intoPackageNamed: packageName2.

	self assert: theClass2 rowanPackageName = Rowan unpackagedName.
	self assert: instanceMethod2 rowanPackageName = packageName2.
	self assert: classMethod2 rowanPackageName = packageName2.

	^(Rowan image loadedProjectNamed: projectName) asDefinition