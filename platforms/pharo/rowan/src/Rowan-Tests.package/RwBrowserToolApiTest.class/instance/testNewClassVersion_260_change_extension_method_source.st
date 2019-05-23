tests
testNewClassVersion_260_change_extension_method_source

	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 
		project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 
		classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass 
		testInstance testNewClass testNewInstance |
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

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	testClass := Rowan globalNamed: className1.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	self assert: testClass foo = 'foo'.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.
	self assert: testInstance foo = 'foo'.

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
					classExtensionDef updateInstanceMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'foo'
										protocol: 'accessing'
										source:  'foo "instance side" ^''foo_''') ] ].

	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	self assert: testClass foo = 'foo'.
	self assert: testInstance ivar1 isNil.
	self assert: testInstance foo = 'foo'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: testNewClass civar1 = 1.
	self should: [ testNewClass cvar1 ] raise: MessageNotUnderstood.
	self assert: testNewClass foo = 'foo'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance ivar1 ] raise: MessageNotUnderstood.
	self assert: testNewInstance foo = 'foo_'.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: 'accessing'
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: 'accessing'
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
