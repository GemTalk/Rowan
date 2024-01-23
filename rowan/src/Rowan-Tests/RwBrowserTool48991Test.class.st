Class {
	#name : 'RwBrowserTool48991Test',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwBrowserTool48991Test >> testAddMethod [
	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool compiledMethod gsNMethod project audit |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageName3 := 'Simple-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	browserTool := RwPrjBrowserTool new.

	"add new method"
	gsNMethod := browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #'ivar1:' with: 3.
	self assert: (testInstance perform: #'ivar1') = 3.
	self should: [ testInstance perform: #'foo' ] raise: MessageNotUnderstood.
	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'ivar1:'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName1.

	"add new extension method"
	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: '*' , packageName2 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.

	self assert: (audit := project audit) isEmpty.
	self assert: (testInstance perform: #'foo') = 'bar'.

	gsNMethod := browserTool
		addOrUpdateMethod: 'foo ^''baz'''
		inProtocol: '*' , packageName2 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	self assert: (audit := project audit) isEmpty.
	self assert: (testInstance perform: #'foo') = 'baz'.

	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'foo'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName2
]

{ #category : 'tests' }
RwBrowserTool48991Test >> testDeleteGlobalExtensionMethod [

	| projectName packageNames className packageName1 testClass testInstance browserTool audit project|
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Global-Extensions1'.
	packageNames := {packageName1}.
	className := 'Object'.
	self
		_loadGlobalExtensionsProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	browserTool := RwPrjBrowserTool new.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: '*', packageName1 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add extension method to Object"

	self assert: (audit := project audit) isEmpty.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	self assert: (testInstance perform: #foo) = 'bar'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className.

	browserTool removeMethod: #'foo' forClassNamed: className isMeta: false.

	self assert: (audit := project audit) isEmpty.

	self should: [ testInstance perform: #foo ] raise: MessageNotUnderstood.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ] ]
		forClassExtensionsIn: className
]

{ #category : 'tests' }
RwBrowserTool48991Test >> testDeleteMethod [

	| projectName packageNames className packageName1 testClass testInstance browserTool testSymDict project audit |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageNames := {packageName1}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.
	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classDef instanceMethodDefinitions size = 1;
						assert: (classDef instanceMethodDefinitions includesKey: #'ivar1') ] ]
		forClassNamed: className.

	browserTool := RwPrjBrowserTool new.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.
	testInstance := testClass new.
	testInstance perform: #ivar1: with: 3.
	self assert: (testInstance  perform: #ivar1) = 3.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classDef instanceMethodDefinitions size = 2;
						assert: (classDef instanceMethodDefinitions includesKey: #'ivar1:') ] ]
		forClassNamed: className.


	browserTool removeMethod: #'ivar1:' forClassNamed: className isMeta: false.

	self assert: (audit := project audit) isEmpty.
	self should: [ testInstance  perform: #ivar1: with: 2 ] raise: MessageNotUnderstood.
	self assert: (testInstance  perform: #ivar1) = 3.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classDef instanceMethodDefinitions size = 1;
						assert: (classDef instanceMethodDefinitions includesKey: #'ivar1') ] ]
		forClassNamed: className
]

{ #category : 'tests' }
RwBrowserTool48991Test >> testMoveMethodBetweenCategories [
	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool compiledMethod gsNMethod project audit |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageName3 := 'Simple-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	browserTool := RwPrjBrowserTool new.

	"add new method"
	gsNMethod := browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #'ivar1:' with: 3.
	self assert: (testInstance perform: #'ivar1') = 3.
	self should: [ testInstance perform: #'foo' ] raise: MessageNotUnderstood.
	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'ivar1:'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName1.
	self assert: (testClass categoryOfSelector: #'ivar1:') equals: #'accessing'.

	"move method to different category"
	gsNMethod := browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'setting'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #'ivar1:' with: 3.
	self assert: (testInstance perform: #'ivar1') = 3.
	self should: [ testInstance perform: #'foo' ] raise: MessageNotUnderstood.
	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'ivar1:'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName1.
	self assert: (testClass categoryOfSelector: #'ivar1:') equals: #'setting'
]

{ #category : 'tests' }
RwBrowserTool48991Test >> testMoveMethodBetweenPackages [

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageName3 := 'HybridA-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = #accessing.
	self assert: (normalClass class categoryOfSelector: #baz) = #accessing.

	 (RwPrjBrowserTool new)
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: 'accessing';
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName2 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = #accessing.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName2 asLowercase) asSymbol.

	(RwPrjBrowserTool new)
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName3 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName3 asLowercase) asSymbol.
]

{ #category : 'tests' }
RwBrowserTool48991Test >> testUpdateMethodSource [
	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool compiledMethod gsNMethod project audit |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageName3 := 'Simple-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	browserTool := RwPrjBrowserTool new.

	"add new method"
	gsNMethod := browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #'ivar1:' with: 3.
	self assert: (testInstance perform: #'ivar1') = 3.
	self should: [ testInstance perform: #'foo' ] raise: MessageNotUnderstood.
	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'ivar1:'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName1.

	"update existing method source"
	gsNMethod := browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := 2 * anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	self assert: (audit := project audit) isEmpty.
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #'ivar1:' with: 3.
	self assert: (testInstance perform: #'ivar1') = 6.
	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'ivar1:'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName1.
]