Extension { #name : 'RwBrowserToolApiTest' }

{ #category : '*rowan-tests-gemstone-36x' }
RwBrowserToolApiTest >> testDeleteComplicatedClass [
	"The class to be deleted is created in one project and extended by another project ... the method extensions are direct method extensions and session method extensions"

	| projectName1 projectName2 packageNames1 packageNames2 className1 className2 packageName1 packageName2 testClass browserTool testProjectDefinition testPackageNames classDefinition testInstance projectTools symDict registry resolvedProject componentName audit project1 project2 x y |
	projectName1 := 'Simple Browser1'.
	componentName := 'Core'.
	packageName1 := 'Simple1-Core'.
	packageName2 := 'Simple1-Extensions'.
	packageNames1 := {packageName1}.
	className1 := 'SimpleBrowse1'.
	className2 := 'SimpleBrowse2'.
	projectName2 := 'Simple Browser2'.
	packageNames2 := {'Simple2-Extensions1'.
	'Simple2-Extensions2'}.
	self
		_loadSimpleProjectDefinition: projectName1
		packageNames: packageNames1
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className1
		inPackageName: packageName1.

	project1 := Rowan projectNamed: projectName1.
	self assert: (audit := project1 audit) isEmpty.

	testClass := Rowan globalNamed: className1.
	self assert: testClass notNil.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addPackageNamed: packageName2
		toComponentNamed: componentName
		andProjectNamed: projectName1.	"new package packageName2"

	packageNames1 := {packageName1.
	packageName2}.
	testProjectDefinition := browserTool projectNamed: projectName1.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames size = packageNames1 size.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'Fun stuff'
		comment: 'just a class'
		pools: #()
		type: 'normal'.
	classDefinition category: packageName2.
	browserTool createClass: classDefinition inPackageNamed: packageName2.	"new class className2"

	testClass := Rowan globalNamed: className2.
	self assert: testClass notNil.

	browserTool
		addOrUpdateMethod: 'foo ^''foo'''
		inProtocol: '*' , packageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: packageName2.	"extension method for className1"

	testClass := Rowan globalNamed: className1.
	self assert: testClass notNil.
	testInstance := testClass new.
	self assert: (testInstance perform: #'foo') = 'foo'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className1.

	self assert: (audit := project1 audit) isEmpty.

	(Rowan image loadedProjectNamed: projectName2 ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		comment: 'another project for testing project browser api';
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		addLoadComponentNamed: componentName;
		addPackagesNamed: packageNames2 toComponentNamed: componentName;
		yourself.

	resolvedProject
		resolveProject;
		load.
	project2 := Rowan projectNamed: projectName2.

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	testProjectDefinition := browserTool projectNamed: projectName2.
	self
		assert:
			(x := testProjectDefinition gemstoneDefaultSymbolDictName)
				= (y := self _symbolDictionaryName1 asString).

	browserTool
		addOrUpdateMethod: 'bar ^''bar'''
		inProtocol: '*' , (packageNames2 at: 1) asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: (packageNames2 at: 1).	"extension method for className1 in projectName2"

	self assert: (testInstance perform: #'bar') = 'bar'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: projectDef name = projectName1;
						assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: projectDef name = projectName1;
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ].
			packageDef name = (packageNames2 at: 1)
				ifTrue: [ 
					self
						assert: projectDef name = projectName2;
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'bar') ] ]
		forClassExtensionsIn: className1.

	self
		_assertForClassExtensionInClassWithHistory: testClass
		ifPresent: [ :classExtensionSet | self assert: classExtensionSet size = 2 ]
		ifAbsent: [ 
			self
				assert: false
				description:
					'Expected class extension for class ' , className1 printString , ' not present' ].
	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: registry packageRegistry size = 2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.

	browserTool removeClassNamed: className1.	"remove class className1"

	self
		_assertForClassExtensionInClassWithHistory: testClass
		ifPresent: [ :classExtensionSet | 
			self
				assert: false
				description:
					'Unexpected class extension for class ' , className1 printString , ' found' ]
		ifAbsent: [ 
			"expected result"
			 ].

	testClass := Rowan globalNamed: className1.
	self assert: testClass isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.

	self assert: registry packageRegistry size = 2.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwBrowserToolApiTest >> testMoveMethod [

	| projectName packageNames className packageName1 packageName2 packageName3 testClass 
		testInstance browserTool testSymDict registry audit project1 |
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

	project1 := Rowan projectNamed: projectName.
	self assert: (audit := project1 audit) isEmpty.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

"validate"
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.
	testInstance := testClass new.
	testInstance perform: #ivar1: with: 3.
	self assert: (testInstance  perform: #ivar1) = 3.
	self should: [ (testInstance  perform: #foo) ] raise: MessageNotUnderstood.
	self _assertNoClassExtensionsIn: className.

	self assert: (audit := project1 audit) isEmpty.
	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: '*', packageName2 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

"validate"
	self assert: (testInstance  perform: #foo) = 'bar'.

	self assert: (audit := project1 audit) isEmpty.
	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ].
			packageDef name = packageName3
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ] ]
		forClassExtensionsIn: className.

	browserTool
		addOrUpdateMethod: 'foo ^''baz'''
		inProtocol: '*', packageName2 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"change class extension method"

"validate"
	self assert: (testInstance  perform: #foo) = 'baz'.

	self assert: (audit := project1 audit) isEmpty.
	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.

	browserTool
		addOrUpdateMethod: 'foo ^''bif'''
		inProtocol: '*', packageName3 asLowercase
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName3.	"move method to another package as a class extension"

"validate"
	self assert: (testInstance  perform: #foo) = 'bif'.

	self assert: (audit := project1 audit) isEmpty.
	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName3
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className
]

{ #category : '*rowan-tests-gemstone-36x' }
RwBrowserToolApiTest >> testRenameClass_Issue_490 [

  "rename a class with no subclasses"

  "https://github.com/dalehenrich/Rowan/issues/490"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition 
		packageDefinition className1 className2 className3 className4 projectSetDefinition
		class1 class2 class3 oldClass1 oldClass2 oldClass3 classExtensionDefinition audit symDict 
		componentName |

  projectName := 'Issue470'. 
  componentName := 'Core'.
  packageName1 := 'Issue490-Core'.
  packageName2 := 'Issue490-Extensions'.
  className1 := 'Issue490Class1'.
  className2 := 'Issue490Class2'.
  className3 := 'Issue490Class3'.
  className4 := 'Issue490Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: {packageName1 . packageName2 } toComponentNamed: componentName;
		yourself.
	projectDefinition resolveProject.

  packageDefinition := projectDefinition packageNamed: packageName1.

  classDefinition1 := (RwClassDefinition
    newForClassNamed: className1
      super: 'Object'
      instvars: #(ivar1)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'instanceMethod1 ^1' protocol: 'accessing');
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod1 ^', className2 protocol: 'accessing');
    yourself.
  packageDefinition
    addClassDefinition: classDefinition1.

  classDefinition := (RwClassDefinition
    newForClassNamed: className2
      super: className1
      instvars: #(ivar2)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'instanceMethod2 ^2' protocol: 'accessing');
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod2 ^', className3 protocol: 'accessing');
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className3
      super: className2
      instvars: #(ivar4 ivar3)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'instanceMethod3 ^3' protocol: 'accessing');
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod3 ^', className1 protocol: 'accessing');
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

"create extension methods"
  packageDefinition := projectDefinition packageNamed: packageName2.

  classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className1)
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionInstanceMethod1 ^1' protocol: '*', packageName2 asLowercase);
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod1 ^1' protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.

  classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className2)
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionInstanceMethod2 ^2' protocol: '*', packageName2 asLowercase);
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod2 ^2' protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.

  classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className3)
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionInstanceMethod3 ^3' protocol: '*', packageName2 asLowercase);
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod3 ^3' protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

  class1 := Rowan globalNamed: className1.
  class2 := Rowan globalNamed: className2.
  class3 := Rowan globalNamed: className3.

"perform rename"
  Rowan projectTools browser
    renameClassNamed: className2 to: className4.

  self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
true ifTrue: [ ^self ].	"early exit for now"

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: oldClass1 == class1.

  class2 := Rowan globalNamed: className4.
  self assert: class2 superClass == class1.
  self assert: oldClass2 ~~ class2. "renamed class"

  class3 := Rowan globalNamed: className3.
  self assert: class3 superClass == class2.
  self assert: oldClass3 ~~ class3. "new version, since superclass renamed"

"audit"
  symDict := Rowan globalNamed: self _symbolDictionaryName1.
  self assert: (symDict at: className4 asSymbol) == class2.
]
