Class {
	#name : 'RwBrowserToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwBrowserToolApiTest >> _expectedByteClassCreationTemplate [
	^ 'SequenceableCollection byteSubclass: ''UnpackagedByteClass''
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanTestData2
	options: #()
'
]

{ #category : 'private' }
RwBrowserToolApiTest >> _expectedClassCreationTemplate [

	^ 'Object rwSubclass: ''NameOfSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ''MyClasses''
	packageName: ''MyPackage''
	options: #()
'
]

{ #category : 'private' }
RwBrowserToolApiTest >> _expectedHybridClassCreationTemplate [

	^ 'Object rwSubclass: ''NameOfSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ''MyClasses''
	options: #()
'
]

{ #category : 'private' }
RwBrowserToolApiTest >> _expectedIndexableCreationTemplate [

	^ 'Object indexableSubclass: ''UnpackagedIndexableClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanTestData2
	options: #()
'
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testAddMethod [
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

	browserTool := Rowan projectTools browser.

	browserTool
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
RwBrowserToolApiTest >> testClassCreationTemplate [

	| template x unpackagedByteClassName unpackagedByteClass unPackagedNormalClass  unpackagedNormalClassName unpackagedIndexableClassName unpackagedIndexableClass |
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.
	unpackagedByteClassName := 'UnpackagedByteClass'.
	unpackagedByteClass := SequenceableCollection byteSubclass: unpackagedByteClassName
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan image symbolDictNamed: self _symbolDictionaryName2)
		options: #().
	unpackagedIndexableClassName := 'UnpackagedIndexableClass'.
	unpackagedIndexableClass := Object indexableSubclass: unpackagedIndexableClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan image symbolDictNamed: self _symbolDictionaryName2)
		options: #().
	unpackagedNormalClassName := 'UnpackagedClass'.
	unPackagedNormalClass := Object subclass: unpackagedNormalClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan image symbolDictNamed: self _symbolDictionaryName2)
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForSubclassOf: 'Object'
		category: 'MyClasses'.
	self
		assert: template = self _expectedHybridClassCreationTemplate
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForSubclassOf: 'Object'
		category: 'MyClasses'
		packageName: 'MyPackage'.
	self
		assert: template = self _expectedClassCreationTemplate
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: unpackagedByteClass
		hybridBrowser: true.
	self
		assert: template = (x := self _expectedByteClassCreationTemplate)
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: unpackagedIndexableClass
		hybridBrowser: false.
	self
		assert: template = self _expectedIndexableCreationTemplate
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: Association
		hybridBrowser: false.
	self
		assert: template = (x := self _expectedAssociationCreationTemplate)
		description: 'incorrect class creation template'
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testCreateClass [

	| projectName packageNames className packageName classDefinition browserTool testClass testSymDict |
	projectName := 'Simple Browser'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	className := 'SimpleBrowse'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testCreateClassWithConstraints [

	| projectName packageNames className packageName classDefinition browserTool testClass testSymDict x |
	projectName := 'Simple Browser'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	className := 'SimpleBrowseWithConstraints'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { {'ivar1' . 'Integer'} }.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.

	self assert: (x := testClass _constraintOn: #ivar1) = Integer

]

{ #category : 'tests' }
RwBrowserToolApiTest >> testCreateClassWithOptions [

	| projectName packageNames className packageName classDefinition browserTool testClass testSymDict |
	projectName := 'Simple Browser'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	className := 'SimpleBrowseWithOptions'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'.
	classDefinition gs_options: #(instancesInvariant).

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.

	self assert: testClass instancesInvariant

]

{ #category : 'tests' }
RwBrowserToolApiTest >> testDeleteClass [

	| projectName packageNames className packageName1 testClass browserTool testSymDict |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageNames := {packageName1}.
	className := 'SimpleBrowse'.
	self
		_loadSimpleProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classDef name = className ] ]
		forClassNamed: className.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.

	browserTool := Rowan projectTools browser.

	browserTool removeClassNamed: className.

	testClass := Rowan globalNamed: className.
	self assert: testClass isNil
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testDeleteGlobalExtensionMethod [

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

	browserTool := Rowan projectTools browser.

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
RwBrowserToolApiTest >> testDeleteMethod [

	| projectName packageNames className packageName1 testClass testInstance browserTool testSymDict |
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

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

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
RwBrowserToolApiTest >> testDeletePackage [

	| projectName packageNames className packageName1 testClass browserTool testSymDict testProjectDefinition testPackageNames |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageNames := {packageName1}.
	className := 'SimpleBrowse'.
	self
		_loadSimpleProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classDef name = className ] ]
		forClassNamed: className.

	browserTool := Rowan projectTools browser.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.
	testProjectDefinition := browserTool projectNamed: projectName.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames = packageNames.

	browserTool removePackageNamed: packageName1.	"remove the package and unload definitions"

	testClass := Rowan globalNamed: className.
	self assert: testClass isNil.
	testProjectDefinition := browserTool projectNamed: projectName.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames isEmpty
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testIssue913 [
	"Efficiently compile a single method"

	"https://github.com/GemTalk/Rowan/issues/913"
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

	browserTool := Rowan globalNamed: #RwPrjBrowserTool.

	browserTool
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
RwBrowserToolApiTest >> testLoadFullMultiProjectDefs [

	"set up projects and packages for hybrid browser implementation"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance rpn |
	projectName1 := 'Simple MultiProject 1'.
	projectName2 := 'Simple MultiProject 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1M-Core'.
	project1PackageName2 := 'Simple1M-Extensions'.
	project2PackageName1 := 'Simple2M-Core'.
	project2PackageName2 := 'Simple2M-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleMultiProject1'.
	className2 := 'SimpleMultiProject2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1M-Core'.
	classPackageNames2 := 'Simple2M-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadFullMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	rpn := Rowan projectNames.
	projectNames do: [ :projectName | self assert: (rpn includes: projectName) ].
	rpn := Rowan packageNames.
	packageNames
		do: [ :packageNameAr | packageNameAr do: [ :packageName | self assert: (rpn includes: packageName) ] ].

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject| 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class session method extension method for className1 in projectName2"

	rpn := Rowan projectNames.
	projectNames do: [ :projectName | self assert: (rpn includes: projectName) ].
	rpn := Rowan packageNames.
	packageNames
		do: [ :packageNameAr | packageNameAr do: [ :packageName | self assert: (rpn includes: packageName) ] ]
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testLoadMultiProjectDefs [

	"set up projects and packages for hybrid browser implementation"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance |
	projectName1 := 'Simple MultiProject 1'.
	projectName2 := 'Simple MultiProject 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1M-Core'.
	project1PackageName2 := 'Simple1M-Extensions'.
	project2PackageName1 := 'Simple2M-Core'.
	project2PackageName2 := 'Simple2M-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleMultiProject1'.
	className2 := 'SimpleMultiProject2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1M-Core'.
	classPackageNames2 := 'Simple2M-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadFullMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2	"class session method extension method for className1 in projectName2"
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testMoveGlobalExtensionSessionMethods [

	| projectName packageNames className packageName1 packageName2 testClass testInstance browserTool project audit|
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Global-Extensions1'.
	packageName2 := 'Simple-Global-Extensions2'.
	packageNames := {packageName1.
	packageName2}.
	className := 'Object'.
	self
		_loadGlobalExtensionsProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: '*', packageName1
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
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ].
			packageDef name = packageName2
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ] ]
		forClassExtensionsIn: className.

	browserTool
		addOrUpdateMethod: 'foo ^''bif'''
		inProtocol: '*', packageName2
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"move method to another package as a class extension"

	self assert: (audit := project audit) isEmpty.

	self assert: (testInstance  perform: #foo) = 'bif'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_260_change_extension_method_protocol [
	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateInstanceMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'foo'
								protocol: 'boom'
								source: 'foo "instance" ^''foo''') ] ].

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self
		assert:
			(testClass categoryOfSelector: #'foo')
				= ('*' , project1PackageName2 asLowercase) asSymbol.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	self assert: (testNewClass categoryOfSelector: #'foo') = #'boom'.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo'.

	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2 asLowercase
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2 asLowercase
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_260_change_extension_method_source [
	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*', project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*', project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateInstanceMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'foo'
								protocol: '*', project1PackageName2 asLowercase
								source: 'foo "instance side" ^''foo_''') ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo_'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*', project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*', project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_260_unchanged_extension_method_protocol [
	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1 ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2 asLowercase
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2 asLowercase
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_multi_project_change_extension_method_protocol [
	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit x |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: false
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self deny: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateInstanceMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'foo'
								protocol: '*' , project1PackageName2
								source: 'foo "instance" ^''foo''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateClassMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'bar'
								protocol: '*' , project2PackageName2
								source: 'bar "class side" ^''bar''') ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.
	self
		assert:
			(x := testClass categoryOfSelector: #'foo')
				= ('*' , project1PackageName2 asLowercase) asSymbol.
	self
		assert:
			(x := testClass class categoryOfSelector: #'bar')
				= ('*' , project2PackageName2 asLowercase) asSymbol.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self
		assert:
			(x := testNewClass categoryOfSelector: #'foo')
				= ('*' , project1PackageName2) asSymbol.
	self
		assert:
			(x := testNewClass class categoryOfSelector: #'bar')
				= ('*' , project2PackageName2) asSymbol.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo'.
	self assert: (testNewInstance perform: #'bar') = 'bar'.

	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2 asLowercase
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2 asLowercase
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self deny: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_multi_project_change_extension_method_source [
	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: false
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self deny: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateInstanceMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'foo'
								protocol: '*' , project1PackageName2
								source: 'foo "instance side" ^''foo_''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateClassMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'bar'
								protocol: '*' , project2PackageName2
								source: 'bar "class side" ^''bar_''') ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testNewClass perform: #'bar') = 'bar_'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo_'.
	self assert: (testNewInstance perform: #'bar') = 'bar'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self deny: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_multi_project_unchanged_extension_method_protocol [
	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: false
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self deny: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1 ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1 ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	self assert: (testNewClass perform: #'bar') = 'bar'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo'.
	self assert: (testNewInstance perform: #'bar') = 'bar'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self deny: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_session_method_change_extension_method_protocol [
	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit x |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateInstanceMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'foo'
								protocol: '*' , project1PackageName2
								source: 'foo "instance" ^''foo''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateClassMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'bar'
								protocol: '*' , project2PackageName2
								source: 'bar "class side" ^''bar''') ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.
	self assert: (x := testClass categoryOfSelector: #'foo') =  ('*' , project1PackageName2) asSymbol.
	self assert: (x := testClass class categoryOfSelector: #'bar') = ('*' , project2PackageName2) asSymbol.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	self assert: (testNewClass perform: #'bar') = 'bar'.
	self assert: (x := testNewClass categoryOfSelector: #'foo') = ('*' , project1PackageName2) asSymbol.
	self assert: (x := testNewClass class categoryOfSelector: #'bar') = ('*' , project2PackageName2) asSymbol.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo'.
	self assert: (testNewInstance perform: #'bar') = 'bar'.

	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_session_method_change_extension_method_source [
	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateInstanceMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'foo'
								protocol: '*' , project1PackageName2
								source: 'foo "instance side" ^''foo_''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef
						updateClassMethodDefinition:
							(RwMethodDefinition
								newForSelector: #'bar'
								protocol: '*' , project2PackageName2
								source: 'bar "class side" ^''bar_''') ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	self assert: (testNewClass perform: #'bar') = 'bar_'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo_'.
	self assert: (testNewInstance perform: #'bar') = 'bar'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_session_method_unchanged_extension_method_protocol [
	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit |
	projectName1 := 'Simple NewVersionD 1'.
	projectName2 := 'Simple NewVersionD 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1D-Core'.
	project1PackageName2 := 'Simple1D-Extensions'.
	project2PackageName1 := 'Simple2D-Core'.
	project2PackageName2 := 'Simple2D-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionD1'.
	className2 := 'SimpleNewVersionD2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1D-Core'.
	classPackageNames2 := 'Simple2D-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1 ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1 ] ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testClass perform: #'foo') = 'foo'.
	self assert: (testClass perform: #'bar') = 'bar'.
	self assert: (testInstance perform: #'ivar1') isNil.
	self assert: (testInstance perform: #'foo') = 'foo'.
	self assert: (testInstance perform: #'bar') = 'bar'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #'foo') = 'foo'.
	self assert: (testNewClass perform: #'bar') = 'bar'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #'foo') = 'foo'.
	self assert: (testNewInstance perform: #'bar') = 'bar'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersionA [
	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance project1 project2 audit |
	projectName1 := 'Simple NewVersionA 1'.
	projectName2 := 'Simple NewVersionA 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1A-Core'.
	project1PackageName2 := 'Simple1A-Extensions'.
	project2PackageName1 := 'Simple2A-Core'.
	project2PackageName2 := 'Simple2A-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionA1'.
	className2 := 'SimpleNewVersionA2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1A-Core'.
	classPackageNames2 := 'Simple2A-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	testInstance := testClass new.
	self assert: (testInstance perform: #'ivar1') isNil.

	browserTool
		classNamed: className1
		updateDefinition: [ :classDef | 
			classDef
				instVarNames: #();
				removeInstanceMethod: #'ivar1';
				classVarNames: #();
				removeClassMethod: #'cvar1';
				updateClassMethodDefinition:
						(RwMethodDefinition
								newForSelector: #'initialize'
								protocol: 'initialization'
								source: 'initialize civar1 := 1.');
				yourself	"shouldn't there be a method to simply add method source and protocol to the class definition?" ].

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: (testClass perform: #'civar1') = 1.
	self assert: (testClass perform: #'cvar1') = 2.
	self assert: (testInstance perform: #'ivar1') isNil.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #'civar1') = 1.
	self should: [ testNewClass perform: #'cvar1' ] raise: MessageNotUnderstood.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #'ivar1' ] raise: MessageNotUnderstood.

	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: '*' , project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersionB [

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen project1 project2 audit |
	projectName1 := 'Simple NewVersionB 1'.
	projectName2 := 'Simple NewVersionB 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1B-Core'.
	project1PackageName2 := 'Simple1B-Extensions'.
	project2PackageName1 := 'Simple2B-Core'.
	project2PackageName2 := 'Simple2B-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionB1'.
	className2 := 'SimpleNewVersionB2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1B-Core'.
	classPackageNames2 := 'Simple2B-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*', project1PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*', project1PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :resolvedProject | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol:  '*', project2PackageName2
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol:  '*', project2PackageName2
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class session method extension method for className1 in projectName2"

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	self assert: (testClass perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.
	self assert: (testInstance perform: #foo) = 'foo'.
	self assert: (testInstance perform: #bar) = 'bar'.


	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self assert: (methodDict includesKey: #'bar').
							sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen.

	browserTool
		projectsNamed: projectNames
		updateDefinition: [ :projectDef | 
			projectDef name = projectName1
				ifTrue: [ 
					| packageDef classDef classExtensionDef |
					packageDef := projectDef packageNamed: project1PackageName1.
					classDef := packageDef classDefinitions at: className1.
					classDef
						instVarNames: #();
						removeInstanceMethod: #'ivar1';
						classVarNames: #();
						removeClassMethod: #'cvar1';
						updateClassMethodDefinition:
								(RwMethodDefinition
										newForSelector: #'initialize'
										protocol: 'initialization'
										source: 'initialize civar1 := 1.');
						yourself.	"shouldn't there be a method to simply add method source and protocol to the class definition?"
					packageDef := projectDef packageNamed: project1PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef removeInstanceMethod: #'foo' ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef removeClassMethod: #'bar' ] ].

	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	self assert: (testClass perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	self assert: (testInstance perform: #ivar1) isNil.
	self assert: (testInstance perform: #foo) = 'foo'.
	self assert: (testInstance perform: #bar) = 'bar'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #civar1) = 1.
	self should: [ testNewClass perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #foo) = 'foo'.
	self should: [ (testNewClass perform: #bar) = 'bar' ] raise: MessageNotUnderstood.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self should: [ (testNewInstance perform: #foo) = 'foo' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #bar) = 'bar'.

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: '*', project1PackageName2
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol:  '*', project1PackageName2
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen
]
