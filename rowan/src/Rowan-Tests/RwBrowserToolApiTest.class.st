Class {
	#name : 'RwBrowserToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwBrowserToolApiTest >> _expectedAssociationCreationTemplate [

	^ 'Object subclass: ''Association''
	instVarNames: #( key value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
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
RwBrowserToolApiTest >> _expectedCollectionCreationTemplate [

	^ 'Object indexableSubclass: ''Collection''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
'

]

{ #category : 'private' }
RwBrowserToolApiTest >> _expectedHybridByteArrayCreationTemplate [

	^ 'SequenceableCollection byteSubclass: ''ByteArray''
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
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

{ #category : 'tests' }
RwBrowserToolApiTest >> testAddMethod [

	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool compiledMethod gsNMethod |
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

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	testInstance perform: #ivar1: with: 3.
	self assert: (testInstance  perform: #ivar1) = 3.
	self should: [ testInstance  perform: #foo ] raise: MessageNotUnderstood.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.

	self assert: (testInstance  perform: #foo) = 'bar'.

	gsNMethod := browserTool
		addOrUpdateMethod: 'foo ^''baz'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	self assert: (testInstance  perform: #foo) = 'baz'.

	self assert: testClass rowanProjectName = projectName.
	self assert: testClass rowanPackageName = packageName1.

	compiledMethod := testClass compiledMethodAt: #'foo'.
	self assert: gsNMethod == compiledMethod.
	self assert: compiledMethod rowanProjectName = projectName.
	self assert: compiledMethod rowanPackageName = packageName2
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testClassCreationTemplate [

	| template x |
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
		classCreationTemplateForClass: ByteArray
		hybridBrowser: true.
	self
		assert: template = (x := self _expectedHybridByteArrayCreationTemplate)
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: ByteArray
		hybridBrowser: false.
	self
		assert: template = (x := self _expectedHybridByteArrayCreationTemplate)
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: Collection
		hybridBrowser: false.
	self
		assert: template = self _expectedCollectionCreationTemplate
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

	| projectName packageNames className packageName1 testClass testInstance browserTool |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Global-Extensions1'.
	packageNames := {packageName1}.
	className := 'Object'.
	self
		_loadGlobalExtensionsProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add extension method to Object"

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
RwBrowserToolApiTest >> testIsExtensionMethod [

	| projectName packageNames className packageName1 packageName2   browserTool |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	browserTool
		addOrUpdateMethod: 'bar ^1'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: true
		inPackageNamed: packageName1.	"add method in the package of the class"

	self deny: (browserTool isExtensionMethod: 'ivar1' forClassNamed: className isMeta: false).
	self deny: (browserTool isExtensionMethod: 'bar' forClassNamed: className isMeta: true).

	browserTool
		addOrUpdateMethod: 'foo ^1'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	browserTool
		addOrUpdateMethod: 'baz ^1'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: true
		inPackageNamed: packageName2.	"add method in a package as a class extension"

	self assert: (browserTool isExtensionMethod: 'foo' forClassNamed: className isMeta: false).
	self assert: (browserTool isExtensionMethod: 'baz' forClassNamed: className isMeta: true).

	self deny: (browserTool isExtensionMethod: 'yourself' forClassNamed: 'Object' isMeta: false). "not rowanized at moment"

]

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: true
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
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: true
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

	| projectName packageNames className packageName1 packageName2 testClass testInstance browserTool |
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

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add extension method to Object"

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
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"move method to another package as a class extension"

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
RwBrowserToolApiTest >> testMoveMethod [

	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool testSymDict
		registry |
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

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 5.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

"validate"
	self assert: (testInstance  perform: #foo) = 'bar'.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

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
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"change class extension method"

"validate"
	self assert: (testInstance  perform: #foo) = 'baz'.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

	browserTool
		addOrUpdateMethod: 'foo ^''bif'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName3.	"move method to another package as a class extension"

"validate"
	self assert: (testInstance  perform: #foo) = 'bif'.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

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

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_260_change_extension_method_protocol [

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
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	self assert: (testClass perform: #foo) = 'foo'.
	testInstance := testClass new.
	self assert: (testInstance perform: #ivar1) isNil.
	self assert: (testInstance perform: #foo) = 'foo'.

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
										source:  'foo "instance" ^''foo''') ] ].

	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.
	self assert: (testClass categoryOfSelector: #foo) = #accessing.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass  perform: #civar1) = 1.
	self should: [ testNewClass  perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testNewClass  perform: #foo) = 'foo'.
	self assert: (testNewClass categoryOfSelector: #foo) = #boom.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance  perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance  perform: #foo) = 'foo'.

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
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_260_change_extension_method_source [

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
	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.

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

	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass  perform: #civar1) = 1.
	self should: [ testNewClass  perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testNewClass  perform: #foo) = 'foo'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance  perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance  perform: #foo) = 'foo_'.
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
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_260_unchanged_extension_method_protocol [

	"https://github.com/dalehenrich/Rowan/issues/260"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 
		project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 
		classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass 
		testInstance testNewClass testNewInstance  |
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
	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.

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

	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass  perform: #civar1) = 1.
	self should: [ testNewClass  perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testNewClass  perform: #foo) = 'foo'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance  perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance  perform: #foo) = 'foo'.
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
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_multi_project_change_extension_method_protocol [

	"https://github.com/dalehenrich/Rowan/issues/260"

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

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: false
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.
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

	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.
	self assert: (testInstance perform: #bar) = 'bar'.
	self assert: (testClass categoryOfSelector: #foo) = #accessing.
	self assert: (testClass class categoryOfSelector: #bar) = #accessing.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testClass  perform: #civar1) = 1.
	self should: [ testNewClass perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	self assert: (testNewClass categoryOfSelector: #foo) = #boom.
	self assert: (testNewClass class categoryOfSelector: #bar) = #boom.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #foo) = 'foo'.
	self assert: (testNewInstance perform: #bar) = 'bar'.

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
	self deny: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_multi_project_change_extension_method_source [

	"https://github.com/dalehenrich/Rowan/issues/260"

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

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: false
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"extension method for className1 in projectName2"

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class extension method for className1 in projectName2"

	testClass := Rowan globalNamed: className1.
	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.
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
					classExtensionDef updateInstanceMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'foo'
										protocol: 'accessing'
										source:  'foo "instance side" ^''foo_''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef updateClassMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'bar'
										protocol: 'accessing'
										source:  'bar "class side" ^''bar_''') ] ].

	self assert: (testClass  perform: #civar1) = 1.
	self assert: (testClass  perform: #cvar1) = 2.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	self assert: (testInstance  perform: #ivar1) isNil.
	self assert: (testInstance  perform: #foo) = 'foo'.
	self assert: (testInstance perform: #bar) = 'bar'.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testClass  perform: #civar1) = 1.
	self should: [ testNewClass perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testClass  perform: #foo) = 'foo'.
	self assert: (testNewClass perform: #bar) = 'bar_'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #foo) = 'foo_'.
	self assert: (testNewInstance perform: #bar) = 'bar'.
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
	self deny: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_multi_project_unchanged_extension_method_protocol [

	"https://github.com/dalehenrich/Rowan/issues/260"

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

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: false
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"extension method for className1 in projectName2"

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2.	"class extension method for className1 in projectName2"

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
	self assert: (testNewClass perform: #bar) = 'bar'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #foo) = 'foo'.
	self assert: (testNewInstance perform: #bar) = 'bar'.
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
	self deny: sessionMethodsSeen
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_session_method_change_extension_method_protocol [

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

	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	self assert: (testClass perform: #foo) = 'foo'.
	self assert: (testClass perform: #bar) = 'bar'.
	self assert: (testInstance perform: #ivar1) isNil.
	self assert: (testInstance perform: #foo) = 'foo'.
	self assert: (testInstance perform: #bar) = 'bar'.
	self assert: (testClass categoryOfSelector: #foo) = #accessing.
	self assert: (testClass class categoryOfSelector: #bar) = #accessing.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #civar1) = 1.
	self should: [ testNewClass perform: #cvar1 ] raise: MessageNotUnderstood.
	self assert: (testNewClass perform: #foo) = 'foo'.
	self assert: (testNewClass perform: #bar) = 'bar'.
	self assert: (testNewClass categoryOfSelector: #foo) = #boom.
	self assert: (testNewClass class categoryOfSelector: #bar) = #boom.

	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #foo) = 'foo'.
	self assert: (testNewInstance perform: #bar) = 'bar'.

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
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_session_method_change_extension_method_source [

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
					classExtensionDef updateInstanceMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'foo'
										protocol: 'accessing'
										source:  'foo "instance side" ^''foo_''') ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1.
					classExtensionDef updateClassMethodDefinition: 
								(RwMethodDefinition
										newForSelector: #'bar'
										protocol: 'accessing'
										source:  'bar "class side" ^''bar_''') ] ].

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
	self assert: (testNewClass perform: #bar) = 'bar_'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #foo) = 'foo_'.
	self assert: (testNewInstance perform: #bar) = 'bar'.
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
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersion_session_method_unchanged_extension_method_protocol [

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
					classExtensionDef := packageDef classExtensions at: className1 ].
			projectDef name = projectName2
				ifTrue: [ 
					| packageDef classExtensionDef |
					packageDef := projectDef packageNamed: project2PackageName2.
					classExtensionDef := packageDef classExtensions at: className1 ] ].

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
	self assert: (testNewClass perform: #bar) = 'bar'.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #foo) = 'foo'.
	self assert: (testNewInstance perform: #bar) = 'bar'.
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
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersionA [

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance |
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

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	testClass := Rowan globalNamed: className1.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	testInstance := testClass new.
	self assert: (testInstance  perform: #ivar1) isNil.

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

	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #cvar1) = 2.
	self assert: (testInstance  perform: #ivar1) isNil.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: (testNewClass perform: #civar1) = 1.
	self should: [ (testNewClass perform: #cvar1) ] raise: MessageNotUnderstood.
	testNewInstance := testNewClass new.
	self should: [ (testNewInstance perform: #ivar1) ] raise: MessageNotUnderstood.

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
		raise: CompileError
]

{ #category : 'tests' }
RwBrowserToolApiTest >> testNewClassVersionB [

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance sessionMethodsSeen |
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
	self assert: (testNewClass perform: #foo = 'foo').
	self should: [ (testNewClass perform: #bar) = 'bar' ] raise: MessageNotUnderstood.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self should: [ (testNewInstance perform: #foo) = 'foo' ] raise: MessageNotUnderstood.
	self assert: (testNewInstance perform: #bar) = 'bar'.
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
]

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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

{ #category : 'tests' }
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
  projectDefinition := (RwProjectDefinition
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
