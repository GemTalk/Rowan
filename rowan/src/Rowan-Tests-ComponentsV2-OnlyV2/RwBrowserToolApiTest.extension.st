Extension { #name : 'RwBrowserToolApiTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testDeleteComplicatedClass [

	"The class to be deleted is created in one project and extended by another project ... the method extensions are direct method extensions and session method extensions"

	| projectName1 projectName2 packageNames1 packageNames2 className1 className2 
		packageName1 packageName2 testClass browserTool testProjectDefinition testPackageNames 
		classDefinition testInstance projectTools symDict registry resolvedProject componentName |
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
	browserTool createClass: classDefinition inPackageNamed: packageName2.	"new class className2"

	testClass := Rowan globalNamed: className2.
	self assert: testClass notNil.

	browserTool
		addOrUpdateMethod: 'foo ^''foo'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: packageName2.	"extension method for className1"

	testClass := Rowan globalNamed: className1.
	self assert: testClass notNil.
	testInstance := testClass new.
	self assert: (testInstance perform: #foo) = 'foo'.

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

	(Rowan image loadedProjectNamed: projectName2 ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		comment:'another project for testing project browser api';
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		addNewComponentNamed: componentName;
		addPackagesNamed: packageNames2 toComponentNamed: componentName;
		yourself.

	resolvedProject
		resolve;
		load.
		
	testProjectDefinition := browserTool projectNamed: projectName2.
	self
		assert:
			testProjectDefinition gemstoneDefaultSymbolDictName = self _symbolDictionaryName1.

	browserTool
		addOrUpdateMethod: 'bar ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: (packageNames2 at: 1).	"extension method for className1 in projectName2"

	self assert: (testInstance  perform: #bar) = 'bar'.

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
		_assertForClassExtensionInClassWithHistory: testClass classHistory
		ifPresent: [ :classExtensionSet | self assert: classExtensionSet size = 2 ]
		ifAbsent: [ 
			self
				assert: false
				description:
					'Expected class extension for class ' , className1 printString , ' not present' ].
	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 2.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

	browserTool removeClassNamed: className1.	"remove class className1"

	self
		_assertForClassExtensionInClassWithHistory: testClass classHistory
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
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testDeleteComplicatedPackage [

	"similar to testDeleteComplicatedClass ... but delete package that contains the class ... should be similar result"

	| projectName1 projectName2 packageNames1 packageNames2 className1 className2 packageName1 
		packageName2 browserTool classDefinition projectTools testClass testProjectDefinition 
		testPackageNames theProjectNames thePackageNames componentName resolvedProject |
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

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool 
		addPackageNamed: packageName2 
		toComponentNamed: componentName 
		andProjectNamed: projectName1.	"new package packageName2"

	packageNames1 := {packageName1.
	packageName2}.

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
	browserTool createClass: classDefinition inPackageNamed: packageName2.	"new class className2"

	browserTool
		addOrUpdateMethod: 'foo ^''foo'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: packageName2.	"extension method for className1"

	(Rowan image loadedProjectNamed: projectName2 ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		comment:'another project for testing project browser api';
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		addNewComponentNamed: componentName;
		addPackagesNamed: packageNames2 toComponentNamed: componentName;
		yourself.

	resolvedProject
		resolve;
		load.

	browserTool
		addOrUpdateMethod: 'bar ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: (packageNames2 at: 1).	"extension method for className1 in projectName2"

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			"validate loaded class extension registry structure"
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

	testClass := Rowan globalNamed: className1.
	self
		_assertForClassExtensionInClassWithHistory: testClass classHistory
		ifPresent: [ :classExtensionSet | self assert: classExtensionSet size = 2 ]
		ifAbsent: [ 
			self
				assert: false
				description:
					'Expected class extension for class ' , className1 printString , ' not present' ].

	theProjectNames := {projectName1.
	projectName2}.
	thePackageNames := packageNames1 , packageNames2.
	theProjectNames
		do: [ :projectName | 
			| theLoadedProject theLoadedPackage theLoadedClassOrClassExtension |
			"Traverse the loaded package structure and verify that it is valid"
			theLoadedProject := Rowan image loadedProjectNamed: projectName.
			theLoadedProject
				loadedPackagesDo: [ :loadedProject :loadedPackage | 
					self assert: theLoadedProject == loadedProject.
					theLoadedPackage := loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					loadedProject name = projectName1
						ifTrue: [ self assert: (packageNames1 includes: loadedPackage name) ].
					loadedProject name = projectName2
						ifTrue: [ self assert: (packageNames2 includes: loadedPackage name) ] ]
				loadedClassedDo: [ :loadedProject :loadedPackage :loadedClass | 
					self assert: theLoadedProject == loadedProject.
					self assert: theLoadedPackage == loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					theLoadedClassOrClassExtension := loadedClass.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ self assert: loadedClass name = className1 ].
							loadedPackage name = packageName2
								ifTrue: [ self assert: loadedClass name = className2 ] ].
					loadedProject name = projectName2
						ifTrue: [ 
							loadedPackage name = (packageNames2 at: 1)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 1) printString ].
							loadedPackage name = (packageNames2 at: 2)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 2) printString ] ] ]
				loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
					self assert: theLoadedProject == loadedProject.
					self assert: theLoadedPackage == loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					theLoadedClassOrClassExtension := loadedClassExtension.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ 
									self
										assert: false
										description: 'no classes expected in ' , packageName1 printString ].
							loadedPackage name = packageName2
								ifTrue: [ self assert: loadedClassExtension name = className1 ] ].
					loadedProject name = projectName2
						ifTrue: [ 
							loadedPackage name = (packageNames2 at: 1)
								ifTrue: [ self assert: loadedClassExtension name = className1 ].
							loadedPackage name = (packageNames2 at: 2)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 2) printString ] ] ]
				loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
					self assert: theLoadedProject == loadedProject.
					self assert: theLoadedPackage == loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					self assert: theLoadedClassOrClassExtension == loadedClassOrClassExtension.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ 
									self
										assert: false
										description: 'no classes expected in ' , packageName1 printString ].
							loadedPackage name = packageName2
								ifTrue: [ self assert: loadedMethod selector = #'foo' ] ].
					loadedProject name = projectName2
						ifTrue: [ 
							loadedPackage name = (packageNames2 at: 1)
								ifTrue: [ self assert: loadedMethod selector = #'bar' ].
							loadedPackage name = (packageNames2 at: 2)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 2) printString ] ] ]
				loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
					self
						assert: false
						description: 'no class methods should be present in the project structure' ] ].


	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ self assert: (methodDict size = 1 and: [ methodDict includesKey: #'bar' ]) ] ] ].

	browserTool removePackageNamed: packageName1.	"remove the package and unload definitions"


	self should: [ Rowan image loadedPackageNamed: packageName1 ] raise: Error.

	GsPackagePolicy current
		packages_Do: [ :gsPackage | 
			gsPackage
				behaviorAndMethodDictDo: [ :beh :methodDict | 
					(beh == testClass or: [ beh class == testClass ])
						ifTrue: [ 
							self
								assert: false
								description:
									'Session methods should not be present for class ' , className1 printString ] ] ].

	self
		_assertForClassExtensionInClassWithHistory: testClass classHistory
		ifPresent: [ :classExtensionSet | 
			self
				assert: false
				description:
					'Unexpected class extension for class ' , className1 printString , ' found' ]
		ifAbsent: [ 
			"expected result"
			 ].

	theProjectNames
		do: [ :projectName | 
			| theLoadedProject theLoadedPackage theLoadedClassOrClassExtension |
			"Traverse the loaded package structure and verify that it is valid"
			theLoadedProject := Rowan image loadedProjectNamed: projectName.
			theLoadedProject
				loadedPackagesDo: [ :loadedProject :loadedPackage | 
					self assert: theLoadedProject == loadedProject.
					theLoadedPackage := loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ 
									self
										assert: false
										description:
											'the package ' , packageName1 , ' should have been removed' ].
							self assert: (packageNames1 includes: loadedPackage name) ].
					loadedProject name = projectName2
						ifTrue: [ self assert: (packageNames2 includes: loadedPackage name) ] ]
				loadedClassedDo: [ :loadedProject :loadedPackage :loadedClass | 
					self assert: theLoadedProject == loadedProject.
					self assert: theLoadedPackage == loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					theLoadedClassOrClassExtension := loadedClass.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ 
									self
										assert: false
										description:
											'the package ' , packageName1 printString
												, ' had been removed no loadedClasses should be present.' ].
							loadedPackage name = packageName2
								ifTrue: [ self assert: loadedClass name = className2 ] ].
					loadedProject name = projectName2
						ifTrue: [ 
							loadedPackage name = (packageNames2 at: 1)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 1) printString ].
							loadedPackage name = (packageNames2 at: 2)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 2) printString ] ] ]
				loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
					self assert: theLoadedProject == loadedProject.
					self assert: theLoadedPackage == loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					theLoadedClassOrClassExtension := loadedClassExtension.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ 
									self
										assert: false
										description: 'no classes expected in ' , packageName1 printString ].
							loadedPackage name = packageName2
								ifTrue: [ 
									self
										assert: false
										description:
											'The class ' , className1 printString
												,
													' was removed when packageName1 was removed - no extensions should be present' ] ].
					loadedProject name = projectName2
						ifTrue: [ 
							loadedPackage name = (packageNames2 at: 1)
								ifTrue: [ 
									self
										assert: false
										description:
											'The class ' , className1 printString
												,
													' was removed when packageName1 was removed - no extensions should be present' ].
							loadedPackage name = (packageNames2 at: 2)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 2) printString ] ] ]
				loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
					self assert: theLoadedProject == loadedProject.
					self assert: theLoadedPackage == loadedPackage.
					self
						assert:
							(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
					self assert: theLoadedClassOrClassExtension == loadedClassOrClassExtension.
					loadedProject name = projectName1
						ifTrue: [ 
							loadedPackage name = packageName1
								ifTrue: [ 
									self
										assert: false
										description: 'no classes expected in ' , packageName1 printString ].
							loadedPackage name = packageName2
								ifTrue: [ 
									self
										assert: false
										description:
											'The method #foo was removed when packageName1 was removed' ] ].
					loadedProject name = projectName2
						ifTrue: [ 
							loadedPackage name = (packageNames2 at: 1)
								ifTrue: [ 
									self
										assert: false
										description:
											'The method #bar was removed when packageName1 was removed' ].
							loadedPackage name = (packageNames2 at: 2)
								ifTrue: [ 
									self
										assert: false
										description:
											'no classes expected in ' , (packageNames2 at: 2) printString ] ] ]
				loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
					self
						assert: false
						description: 'no class methods should be present in the project structure' ] ].

	testClass := Rowan globalNamed: className1.
	self assert: testClass isNil.
	testProjectDefinition := browserTool projectNamed: projectName1.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames = {packageName2}

]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testIssue471_1 [

	"expected failure until issue #471 is addressed"

  "https://github.com/dalehenrich/Rowan/issues/471"

  | projectName  packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 class4 oldClass1 oldClass2 oldClass3  |

  projectName := 'Issue471'.
  packageName1 := 'Issue471-Core'.
  packageName2 := 'Issue471-Extensions'.
  className1 := 'Issue471Class1'.
  className2 := 'Issue471Class2'.
  className3 := 'Issue471Class3'.
  className4 := 'Issue471Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

  packageDefinition := projectDefinition packageNamed: packageName1.

  classDefinition := (RwClassDefinition
    newForClassNamed: className1
      super: 'Object'
      instvars: #(ivar1)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className2
      super: className1
      instvars: #('ivar2')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className3
      super: className2
      instvars: #('ivar4' 'ivar3')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
   yourself.
  packageDefinition
    addClassDefinition: classDefinition.

"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: class3 superClass == class2.

"remove class2 and add class4 -- edit projectDefinition structure in place"
  packageDefinition := projectDefinition packageNamed: packageName1.

  packageDefinition removeClassNamed: className2.

  classDefinition := (RwClassDefinition
    newForClassNamed: className4
      super: className1
      instvars: #('ivar2')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.
"load"
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: oldClass1 == class1.

  class4 := Rowan globalNamed: className4.
  self assert: class4 instVarNames = #(ivar2).
  self assert: class4 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: class3 superClass == oldClass2.
  self assert: oldClass3 == class3.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testIssue471_2 [

  "https://github.com/dalehenrich/Rowan/issues/471"

  | projectName  packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 class4 oldClass1 oldClass2 oldClass3  |

  projectName := 'Issue471'.
  packageName1 := 'Issue471-Core'.
  packageName2 := 'Issue471-Extensions'.
  className1 := 'Issue471Class1'.
  className2 := 'Issue471Class2'.
  className3 := 'Issue471Class3'.
  className4 := 'Issue471Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

  packageDefinition := projectDefinition packageNamed: packageName1.

  classDefinition := (RwClassDefinition
    newForClassNamed: className1
      super: 'Object'
      instvars: #(ivar1)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className2
      super: className1
      instvars: #('ivar2')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className3
      super: className2
      instvars: #('ivar4' 'ivar3')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
   yourself.
  packageDefinition
    addClassDefinition: classDefinition.

"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: class3 superClass == class2.

"remove class2 and add class4 -- edit projectDefinition structure in place"
  projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
  self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectLoadedDefinitionSourceValue).

  packageDefinition := projectDefinition packageNamed: packageName1.

  packageDefinition removeClassNamed: className2.

  classDefinition := (RwClassDefinition
    newForClassNamed: className4
      super: className1
      instvars: #('ivar2')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.
"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: oldClass1 == class1.

  class4 := Rowan globalNamed: className4.
  self assert: class4 instVarNames = #(ivar2).
  self assert: class4 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: class3 superClass == oldClass2.
  self assert: oldClass3 == class3.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testRenameClass_1 [

  "rename a class with no subclasses"

  "https://github.com/dalehenrich/Rowan/issues/470"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 classExtensionDefinition |

  projectName := 'Issue470'.
  packageName1 := 'Issue470-Core'.
  packageName2 := 'Issue470-Extensions'.
  className1 := 'Issue470Class1'.
  className2 := 'Issue470Class2'.
  className3 := 'Issue470Class3'.
  className4 := 'Issue470Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

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

"validate"
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2.

"perform rename"
  Rowan projectTools browser
    renameClassNamed: className3 to: className4.

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.
  self assert: oldClass1 == class1.

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.
  self assert: oldClass2 == class2.

  class3 := Rowan globalNamed: className4.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2.
  self assert: oldClass3 ~~ class3. "renamed class"
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testRenameClass_2 [

  "rename a class with a subclass - reference to renamed class in methods"

  "https://github.com/dalehenrich/Rowan/issues/470"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 class4 oldClass1 oldClass2 oldClass3 classExtensionDefinition |

  projectName := 'Issue470'.
  packageName1 := 'Issue470-Core'.
  packageName2 := 'Issue470-Extensions'.
  className1 := 'Issue470Class1'.
  className2 := 'Issue470Class2'.
  className3 := 'Issue470Class3'.
  className4 := 'Issue470Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

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

"validate"
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2.

"perform rename"
  Rowan projectTools browser
    renameClassNamed: className2 to: className4.

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.
  self assert: oldClass1 == class1.

  class4 := Rowan globalNamed: className4.
  self assert: class4 instVarNames = #(ivar2).
  self assert: (class4 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class4 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class4 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class4 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class4 superClass == class1.
  self assert: oldClass2 ~~ class4. "renamed class"

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class4.
  self assert: oldClass3 ~~ class3. "subclass of renamed class"
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testRenameClass_3 [

  "rename a class with a subclass - no references to renamed class in methods"

  "https://github.com/dalehenrich/Rowan/issues/470"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 class4 oldClass1 oldClass2 oldClass3 classExtensionDefinition |

  projectName := 'Issue470'.
  packageName1 := 'Issue470-Core'.
  packageName2 := 'Issue470-Extensions'.
  className1 := 'Issue470Class1'.
  className2 := 'Issue470Class2'.
  className3 := 'Issue470Class3'.
  className4 := 'Issue470Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod1 ^1' protocol: 'accessing');
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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod2 ^2' protocol: 'accessing');
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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod3 ^3' protocol: 'accessing');
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

"validate"
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2.

"perform rename"
  Rowan projectTools browser
    renameClassNamed: className2 to: className4.

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.
  self assert: oldClass1 == class1.

  class4 := Rowan globalNamed: className4.
  self assert: class4 instVarNames = #(ivar2).
  self assert: (class4 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class4 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class4 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class4 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class4 superClass == class1.
  self assert: oldClass2 ~~ class4. "renamed class"

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class4.
  self assert: oldClass3 ~~ class3. "subclass of renamed class"
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testRenameClass_4 [

  "rename a class with a subclass - reference to renamed class in methods subclass references old superclass in method"

  "https://github.com/dalehenrich/Rowan/issues/470"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 classExtensionDefinition validateBlock |

  projectName := 'Issue470'.
  packageName1 := 'Issue470-Core'.
  packageName2 := 'Issue470-Extensions'.
  className1 := 'Issue470Class1'.
  className2 := 'Issue470Class2'.
  className3 := 'Issue470Class3'.
  className4 := 'Issue470Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod3 ^', className2 protocol: 'accessing');
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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod3 ^', className1 protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.


"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  validateBlock := [:oldClass1 :oldClass2 :oldClass3 |
  class1 := Rowan globalNamed: className1.
  self assert: class1 == oldClass1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.

  class2 := Rowan globalNamed: className2.
  self assert: class2 == oldClass2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 == oldClass3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2 ].

   validateBlock value: (Rowan globalNamed: className1) value: (Rowan globalNamed: className2) value: (Rowan globalNamed: className3).

"perform rename --- expect to fail with compile error at the moment"
	self should: [
		Rowan projectTools browser
			renameClassNamed: className2 to: className4 ]
		raise: CompileError.

"validate"
  validateBlock value: class1 value: class2 value: class3.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testRenameClass_5 [

  "rename a class with a subclass - reference to renamed class in methods subclass references old superclass in extension method"

  "https://github.com/dalehenrich/Rowan/issues/470"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 validateBlock classExtensionDefinition |

  projectName := 'Issue470'.
  packageName1 := 'Issue470-Core'.
  packageName2 := 'Issue470-Extensions'.
  className1 := 'Issue470Class1'.
  className2 := 'Issue470Class2'.
  className3 := 'Issue470Class3'.
  className4 := 'Issue470Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod3 ^', className2 protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.


"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  validateBlock := [:oldClass1 :oldClass2 :oldClass3 |
  class1 := Rowan globalNamed: className1.
  self assert: class1 == oldClass1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.

  class2 := Rowan globalNamed: className2.
  self assert: class2 == oldClass2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 == oldClass3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2 ].

   validateBlock value: (Rowan globalNamed: className1) value: (Rowan globalNamed: className2) value: (Rowan globalNamed: className3).

"perform rename"
	self should: [ 
		Rowan projectTools browser
			renameClassNamed: className2 to: className4]
		raise: CompileError.

"validate"
   validateBlock value: class1 value: class2 value: class3.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolApiTest >> testRenameClass_Issue_490 [

  "rename a class with no subclasses"

  "https://github.com/dalehenrich/Rowan/issues/490"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition 
		packageDefinition className1 className2 className3 className4 projectSetDefinition
		class1 class2 class3 oldClass1 oldClass2 oldClass3 classExtensionDefinition audit symDict 
		registry extraMethods methodRegistry seenMethodsMap |

  projectName := 'Issue470'.
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
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
    newForGitBasedProjectNamed: projectName)
    addPackageNamed: packageName1;
    addPackageNamed: packageName2;
    defaultSymbolDictName: self _symbolDictionaryName1;
    yourself.

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
  registry := symDict at: #RwSymbolDictionaryRegistry.
  methodRegistry := registry methodRegistry.
  seenMethodsMap := Dictionary new.
  methodRegistry keysAndValuesDo: [:method :loadedMethod |
    ((seenMethodsMap at: method inClass name ifAbsentPut: [ Dictionary new])
		at: method selector ifAbsentPut: [ {} ]) add: method ].
  extraMethods := Dictionary new.
  seenMethodsMap keysAndValuesDo: [:className :selectorMap |
    selectorMap keysAndValuesDo: [:selector :ar |
      ar size > 1 
        ifTrue: [
          (extraMethods at: className ifAbsentPut: [ Dictionary new ])
            at: selector put: ar ] ] ].
  true ifTrue: [ self assert: extraMethods size = 0 ].
 false ifTrue: [ "repair"
  extraMethods keysAndValuesDo: [:className :selectorMap |
    selectorMap keysAndValuesDo: [:selector :ar |
      ar do: [:method |
        | theClass x|
        theClass := method inClass.
         theClass :=theClass isMeta
           ifTrue: [ (Rowan globalNamed: theClass theNonMetaClass name) class ]
           ifFalse: [ Rowan globalNamed: theClass name ].
        (x := theClass compiledMethodAt: selector) == method
          ifFalse: [ methodRegistry removeKey: method ] ] ] ].

  seenMethodsMap := Dictionary new.
  methodRegistry keysAndValuesDo: [:method :loadedMethod |
    ((seenMethodsMap at: method inClass name ifAbsentPut: [ Dictionary new])
		at: method selector ifAbsentPut: [ {} ]) add: method ].
  extraMethods := Dictionary new.
  seenMethodsMap keysAndValuesDo: [:className :selectorMap |
    selectorMap keysAndValuesDo: [:selector :ar |
      ar size > 1 
        ifTrue: [
          (extraMethods at: className ifAbsentPut: [ Dictionary new ])
            at: selector put: ar ] ] ].
  true ifTrue: [ self assert: extraMethods size = 0 ] ].
]
