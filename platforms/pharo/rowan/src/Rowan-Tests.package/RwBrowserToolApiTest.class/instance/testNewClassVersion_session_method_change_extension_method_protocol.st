tests
testNewClassVersion_session_method_change_extension_method_protocol

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 
		project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 
		classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass 
		testInstance testNewClass testNewInstance sessionMethodsSeen |
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

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class session method extension method for className1 in projectName2"

	testClass := Rowan globalNamed: className1.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	self assert: testClass foo = 'foo'.
	self assert: testClass bar = 'bar'.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.
	self assert: testInstance foo = 'foo'.
	self assert: testInstance bar = 'bar'.


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
					classExtensionDef updateInstanceMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'foo'
										protocol: 'boom'
										source:  'foo "instance" ^''foo''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef updateClassMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'bar'
										protocol: 'boom'
										source:  'bar "class side" ^''bar''') ] ].

	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	self assert: testClass foo = 'foo'.
	self assert: testClass bar = 'bar'.
	self assert: testInstance ivar1 isNil.
	self assert: testInstance foo = 'foo'.
	self assert: testInstance bar = 'bar'.
	self assert: (testClass categoryOfSelector: #foo) = #accessing.
	self assert: (testClass class categoryOfSelector: #bar) = #accessing.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: testNewClass civar1 = 1.
	self should: [ testNewClass cvar1 ] raise: MessageNotUnderstood.
	self assert: testNewClass foo = 'foo'.
	self assert: testNewClass bar = 'bar'.
	self assert: (testNewClass categoryOfSelector: #foo) = #boom.
	self assert: (testNewClass class categoryOfSelector: #bar) = #boom.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance ivar1 ] raise: MessageNotUnderstood.
	self assert: testNewInstance foo = 'foo'.
	self assert: testNewInstance bar = 'bar'.

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

	sessionMethodsSeen := false.
	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testNewClass or: [ beh class == testNewClass ])
						ifTrue: [ sessionMethodsSeen := true ] ] ].
	self assert: sessionMethodsSeen
