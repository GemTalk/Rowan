"
keep test cases here
"
Class {
	#name : 'RwProjectAuditToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectAuditToolTest >> _auditBlock [

	|  res  loadedProject auditLoadedClassBlock auditLoadedClassExtensionBlock |
	GsFile gciLogServer: '--STARTING ROWAN AUDIT'.
	res := KeyValueDictionary new.
	auditLoadedClassBlock := self _auditLoadedClassBlock.

	auditLoadedClassExtensionBlock := self _auditLoadedClassBlock.
		
	Rowan projectNames do: [:projectName |
		GsFile gciLogServer: '---Auditing project: ', projectName printString.
		loadedProject := Rowan image loadedProjectNamed: projectName.
		loadedProject loadedPackages do: [:loadedPackage |
			loadedPackage
				loadedClasses
					valuesDo: [:aLoadedClass |
						(auditLoadedClassBlock value: aLoadedClass) 
							ifNotEmpty: [:aColl | res at: aLoadedClass name put: aColl ] ].
			loadedPackage
				loadedClassExtensions
					valuesDo: [:aLoadedClass | 
						(auditLoadedClassExtensionBlock value: aLoadedClass) 
							ifNotEmpty: [:aColl | res at: aLoadedClass name put: aColl ] ] ] ].
	UserGlobals at: #ROWAN_AUDIT_issue_365_results put: res.
	GsFile gciLogServer: '--ENDING ROWAN AUDIT'.
^true

]

{ #category : 'tests' }
RwProjectAuditToolTest >> _auditLoadedClassBlock [ 

	^[:aLoadedClass |
		| ar |
		ar := {}.
		(Rowan globalNamed: aLoadedClass name) 
			ifNil: [
				ar addAll: { 'Class does not exists ' -> aLoadedClass name }.
				GsFile gciLogServer: 'Class does not exist ', aLoadedClass name ]
			ifNotNil: [:aBehavior |
				"check for non-extension methods that are not packaged in Rowan" 
				(aBehavior selectors reject: [:e |  
					((aBehavior categoryOfSelector: e) first == $*) or: [
						aLoadedClass includesSelector: e isMeta: false]]) 
							do: [:aSelector |
								ar addAll: { aSelector  -> ' Missing loaded method '  }.
								GsFile gciLogServer: 'Missing loaded method ', aLoadedClass name, '>>', aSelector ].
				(aBehavior class selectors reject: [:e | 
					((aBehavior class categoryOfSelector: e) first == $*) or: [
						aLoadedClass includesSelector: e isMeta: true]]) 
							do: [:aSelector |
								ar addAll: {aSelector  -> 'Missing loaded class method ' }.
								GsFile gciLogServer: 'Missing loaded method ', aLoadedClass name, ' class>>', aSelector ].
				"look for methods removed outside of Rowan API"
				aLoadedClass 
					loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
						(aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: { aLoadedMethod name  -> 'Missing compiled method: ' }.
								GsFile gciLogServer: 'Missing compiled method ', loadedClass name, '>>', aLoadedMethod name ] ]
					loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
						(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: { aLoadedMethod name  -> 'Missing compiled class method ' }.
								GsFile gciLogServer: 'Missing compiled method ', loadedClass name, ' class>>', aLoadedMethod name ] ] ].
		ar ].
]

{ #category : 'tests' }
RwProjectAuditToolTest >> test_issue478 [

	"https://github.com/GemTalk/Rowan/issues/478"

	| projectName packageNames className packageName1 packageName2 theClass  audit |
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extensions'.
	packageNames := {packageName1 .  packageName2}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	theClass
		rwCompileMethod: 'bar ^bar'
		category: '*' , packageName2 asLowercase.
	 	 
	theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , 'audit-extenSions'.

	self assert: (theClass compiledMethodAt: #bar) rowanPackageName = packageName2.
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.

	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testAuditAndRepair_issue481_A [

	"https://github.com/GemTalk/Rowan/issues/481"

	"extra instance method for packaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName audit symbolList |

	symbolList := Rowan image symbolList.

	projectName := 'AuditProject_481'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AuditedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create loadedProject"
	self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"add extra instance method"
	(Rowan globalNamed: className1)
		compileMethod: 'foobar ^''foobar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

"audit - audit errors expected"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"audit - repair audit errors"
	[audit := Rowan projectTools audit auditForProjectNamed: projectName ]
		on: RwAuditMethodErrorNotification
		do: [:ex | 
			"adopt the method"
			Rowan packageTools adopt 
				adoptMethod: ex selector 
					inClassNamed: ex className  
					isMeta: ex isMetaclass 
					intoPackageNamed: ex packageName.
			ex resume: false ].
	self assert: audit isEmpty.

"double check audit - pass"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testAuditAndRepair_issue481_B [

	"https://github.com/GemTalk/Rowan/issues/481"

	"extra class method for packaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName audit symbolList |

	symbolList := Rowan image symbolList.

	projectName := 'AuditProject_481'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AuditedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create loadedProject"
	self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"add extra class method"
	(Rowan globalNamed: className1) class
		compileMethod: 'foobar ^''foobar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

"audit - audit errors expected"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"audit - repair audit errors"
	[audit := Rowan projectTools audit auditForProjectNamed: projectName ]
		on: RwAuditMethodErrorNotification
		do: [:ex | 
			"adopt the method"
			Rowan packageTools adopt 
				adoptMethod: ex selector 
					inClassNamed: ex className  
					isMeta: ex isMetaclass 
					intoPackageNamed: ex packageName.
			ex resume: false ].
	self assert: audit isEmpty.

"double check audit - pass"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testAuditAndRepair_issue481_C [
	"https://github.com/GemTalk/Rowan/issues/481"

	"extra instance method for unpackaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName audit symbolList |

	symbolList := Rowan image symbolList.

	projectName := 'AuditProject_481'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AuditedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create loadedProject"
	self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"add extra instance method"
	(Rowan globalNamed: className2)
		compileMethod: 'foobar ^''foobar'''
		dictionaries: symbolList
		category: '*' , packageName2 asLowercase
		environmentId: 0.

"audit - audit errors expected"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"audit - repair audit errors"
	[audit := Rowan projectTools audit auditForProjectNamed: projectName ]
		on: RwAuditMethodErrorNotification
		do: [:ex | 
			"adopt the method"
			Rowan packageTools adopt 
				adoptMethod: ex selector 
					inClassNamed: ex className  
					isMeta: ex isMetaclass 
					intoPackageNamed: ex packageName.
			ex resume: false ].
	self assert: audit isEmpty.

"validate that method is packaged properly"
	self assert: ((Rowan globalNamed: className2) compiledMethodAt: #foobar) rowanPackageName = packageName2.

"double check audit - pass"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testAuditAndRepair_issue481_D [
	"https://github.com/GemTalk/Rowan/issues/481"

	"extra class method for unpackaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName audit symbolList |

	symbolList := Rowan image symbolList.

	projectName := 'AuditProject_481'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AuditedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create loadedProject"
	self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"add extra instance method"
	(Rowan globalNamed: className2) class
		compileMethod: 'foobar ^''foobar'''
		dictionaries: symbolList
		category: '*' , packageName2 asLowercase
		environmentId: 0.

"audit - audit errors expected"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"audit - repair audit errors"
	[audit := Rowan projectTools audit auditForProjectNamed: projectName ]
		on: RwAuditMethodErrorNotification
		do: [:ex | 
			"adopt the method"
			Rowan packageTools adopt 
				adoptMethod: ex selector 
					inClassNamed: ex className  
					isMeta: ex isMetaclass 
					intoPackageNamed: ex packageName.
			ex resume: false ].
	self assert: audit isEmpty.

"validate that method is packaged properly"
	self assert: ((Rowan globalNamed: className2) class compiledMethodAt: #foobar) rowanPackageName = packageName2.

"double check audit - pass"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testClassBadExtensionName [

| packageTools projectName packageNames className packageName1 packageName2 theClass fooMethod x y|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extensions'.
	packageNames := {packageName1 . packageName2}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: theClass rowanPackageName = packageName1.

   theClass 
		rwCompileMethod: 'bar ^bar'
	    category: 'Accessing' "'*'  packageName1 asLowercase".

	fooMethod := theClass
		rwCompileExtensionMethod: 'foo ^''foo'''
		package: packageName2.

self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) isEmpty.
 
 "lets break it"
	fooMethod := theClass
		compileMethod: 'foo ^2'
		dictionaries: Rowan image symbolList
		category: ('*' , packageName1 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 2.
	self assert: ( y := x at: packageName1) notNil; 
		assert: y size = 1;
		assert: (y at: className) size = 1;
		assert: (y := x at: packageName2 ) notNil;
		assert: y size = 1;
		assert: (y at: className) size = 1
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testClassExtension [

| packageTools projectName packageNames className packageName1 packageName2 packageName3 theClass fooMethod x y|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extensions'.
	packageName3 := 'Audit-Class-Extensions'.
	packageNames := {packageName1 . packageName2 . packageName3}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: theClass rowanPackageName = packageName1.
	"self assert: theClass classHistory isEmpty."
	"compile good class"
   theClass class 
		rwCompileMethod: 'new ^super new '
		category: 'Instance creation'.
	
	theClass class
		rwCompileExtensionMethod: 'new2 ^self new'
		package: packageName2.

	theClass class
		rwCompileExtensionMethod: 'new3 ^self new initialize'
		package: packageName3.

   theClass 
		rwCompileMethod: 'bar ^bar'
	    category: 'Accessing' "'*'  packageName1 asLowercase".

	fooMethod := theClass
		rwCompileExtensionMethod: 'foo ^''foo'''
		package: packageName2.
	
x := Rowan projectTools audit auditForProjectNamed:  'AuditProject'.


self assert: x isEmpty.

	fooMethod := theClass
		compileMethod: 'bar: aBar bar := aBar'
		dictionaries: Rowan image symbolList
		category: ('*' , packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	fooMethod := theClass class
		compileMethod: 'new2 ^self new'
		dictionaries: Rowan image symbolList
		category: ('*' , packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	fooMethod := theClass class
		compileMethod: 'new3 ^self new initialize'
		dictionaries: Rowan image symbolList
		category: ('*' , packageName3 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

	fooMethod := theClass class
		compileMethod:  'new ^super new '
		dictionaries: Rowan image symbolList
		category: #'*Instance creation' "non existing extension/package"
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.

x := Rowan projectTools audit auditForProjectNamed:  'AuditProject'.


	self 
	  assert: x size = 3;
	  assert: ((y := x at: packageName1) at: className) size = 4;
	  assert: ((y := x at: packageName2) at: className) size = 3;
	  assert: ((y := x at: packageName3) at: className) size = 2
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testClassVars [
	| projectName packageNames className packageName classDefinition browserTool testClass testClassB  x |
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
    className := 'ClassWithVars'.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1' 'ivar2')
		classinstvars: #('civar1' 'civar2')
		classvars: #('Cvar1' 'Cvar2')
		category: 'Simple Things'
		comment: 'I am a Simple class with various vars'
		pools: #()
		type: 'normal'.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') isEmpty.
	
	testClassB := DateTime subclass: className
	instVarNames: #('ivar1' 'ivar3' )
	classVars: #( 'Cvar1' 'Cvar3')
	classInstVars: #('civar3')
	poolDictionaries: #()
	inDictionary: (System myUserProfile resolveSymbol: self _symbolDictionaryName) value
	newVersionOf: testClass
	description: 'Unrowanized class'
	options: #().

	self assert: testClassB == (System myUserProfile resolveSymbol: className asSymbol) value.	
	self assert: testClassB == (Rowan globalNamed: className) description: 'Rowan does not resolve new version of class'.
	self deny: testClassB == testClass.
	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') isEmpty
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testInvalidCategory [

| packageTools projectName packageNames className packageName theClass  x category|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.
	category := 'Accessing'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	theClass
		rwCompileMethod: 'bar ^bar'
		category: category.
	 
	theClass class
		rwCompileMethod: 'new ^self basicNew'
		category: 'Instance creation'.
	 
	theClass renameCategory: category to: 'testing new category'.
	theClass class renameCategory: 'Instance creation' to: 'testing'.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 1.
	self assert: ((x at: packageName) at: className) size = 2

]

{ #category : 'tests' }
RwProjectAuditToolTest >> testInvalidExtensionCategory [

| packageTools projectName packageNames className packageName theClass  x|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	theClass
		rwCompileExtensionMethod: 'bar ^bar'
		package: "'*' , "packageName asLowercase.
	 
	theClass class
		rwCompileExtensionMethod: 'new ^self basicNew'
		package: "'*' , "packageName asLowercase.
	 
	theClass class
		rwCompileMethod: 'new2 ^self basicNew'
		category: '*' , packageName.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 1.
	self assert: ((x at: packageName) at: className) size = 3
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testMissingBehavior [

| packageTools projectName packageNames className packageName theClass  x|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	
	(System myUserProfile resolveSymbol: self _symbolDictionaryName1) value removeKey: theClass name asSymbol.
	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName) at: className) size = 2
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testMissingMethods [

| packageTools projectName packageNames className packageName theClass fooMethod x|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: "'*' , "packageName asLowercase.
	 
	fooMethod := theClass class
		compileMethod: 'new ^super new'
		dictionaries: #()
		category: 'Instance Creation'.

	theClass compileMissingAccessingMethods. "this should add: #bar #bar:"


	((Rowan image loadedPackageNamed: 'Audit-Core') loadedClassForClass: (GsSession currentSession resolveSymbol: className asSymbol) value ifAbsent: [nil error: 'expected a class']) 
			loadedInstanceMethodsDo: [:anLMethod | (anLMethod name isEquivalent: 'bar') ifTrue: [self error: 'bar was compiled without Rowan']] 
			loadedClassMethodsDo: [:cMethod | "do nothing"].

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName) at: className) size = 3

]

{ #category : 'tests' }
RwProjectAuditToolTest >> testNonIdenticalMethodAndMissingLoadedMethodUpgradeRepair [
	"It is possible to get two VALID audit errors for the same method, 
		which complicates the algorithm used to repair the audit method
		during upgrade (see internal bugs 49709 and 49814). This test
		aims to reproduce the audit errors and validate the audit algorthm.
	"

	"related to https://github.com/GemTalk/Rowan/issues/799"

	| packageTools projectName packageNames className packageName1 packageName2 theClass audit componentName definedProject reasons loadedClass theMethod loadedMethod candidateMap |
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 . packageName2 . }.
	className := 'AuditClass'.
	componentName := 'Core'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addTopLevelComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		packageConvention: 'RowanHybrid';
		yourself.
	definedProject resolve load.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.
"add a method and create Rowan metadata"
	theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*', packageName2 asLowercase.
	theClass class
		rwCompileMethod: 'foo ^''foo'''
		category: '*', packageName2 asLowercase.
"simulate upgradeImage on a kernel class (i.e., remove method and compile new method without touching Rowan metadata"
"1. remove method from theClass, leaving the oldLoadedMethod intact"
	theClass removeSelector: #foo.
	theClass class removeSelector: #foo.
"2. install a new compiledMethod leaving the oldLoadedMethod intact"
	theClass
		compileMethod:  'foo ^''foo'''
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
	theClass class
		compileMethod:  'foo ^''foo'''
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
"validate that we've got 'Missing loaded method' and 'compiled method is not identical to loaded class method' audit errors"
	audit := Rowan projectTools audit auditForProjectNamed:  projectName.
	reasons := (((audit at: packageName2) at: className) collect: [:auditDetail | auditDetail reason ]) asArray sort.
	self assert: reasons equals: #(methodsNotIdentical missingLoadedMethod methodsNotIdentical missingLoadedMethod) sort.
"analyze"
	candidateMap := IdentityKeyValueDictionary new.
	audit keysAndValuesDo: [:packageName :auditReport |
		auditReport keysAndValuesDo: [:theClassName :auditDetails |
			| methodsNotIdenticalMap |
			methodsNotIdenticalMap := IdentityKeyValueDictionary new.
			(auditDetails select: [:auditDetail | auditDetail reason == #methodsNotIdentical ])
				do: [:auditDetail | 
					(methodsNotIdenticalMap at: auditDetail behavior ifAbsentPut: [SymbolKeyValueDictionary new])
						at: auditDetail selector put: auditDetail].	
			(auditDetails select: [:auditDetail | auditDetail reason == #missingLoadedMethod]) 
				do: [:auditDetail | 
					((methodsNotIdenticalMap includesKey: auditDetail behavior) and: [(methodsNotIdenticalMap at: auditDetail behavior) includesKey: auditDetail selector])
						ifTrue: [
							((candidateMap at: auditDetail behavior ifAbsentPut: [ SymbolKeyValueDictionary new])
								at: auditDetail selector ifAbsentPut: [Dictionary new])
									at: #missingLoadedMethodDetail put: auditDetail;
									at: #methodsNotIdenticalDetail put: ((methodsNotIdenticalMap at: auditDetail behavior) at: auditDetail selector) ] ] ] ].
	"repair using candidate map"
	candidateMap keysAndValuesDo: [:theBehavior :selectorDict |
		selectorDict keysAndValuesDo: [:theSelector :detailDict |
			| methodsNotIdenticalDetail  |
			methodsNotIdenticalDetail := detailDict at: #methodsNotIdenticalDetail.
			loadedClass :=  (Rowan image loadedClassExtensionsForClass: theBehavior theNonMetaClass) any.
			theMethod := theBehavior compiledMethodAt: theSelector.
			self assert: theMethod == methodsNotIdenticalDetail method.
			loadedMethod := theBehavior isMeta
				ifTrue: [ loadedClass loadedClassMethods at: theSelector ]
				ifFalse: [ loadedClass loadedInstanceMethods at: theSelector ].
			self assert: loadedMethod == methodsNotIdenticalDetail loadedMethod.
			loadedMethod handle: theMethod.
			theMethod _rowanPackageInfo: loadedMethod ] ].

"validate"
	audit := Rowan projectTools audit auditForProjectNamed:  projectName.
	self assert: audit isEmpty
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testNonIdenticalMethods_1 [
	"Two 'method is not identical' audit errors:
		RwClsAuditTool>>_auditRowanCategory:forBehavior:loadedClass:
		RwClsExtensionAuditTool>>auditLoadedClassExtension:
	"

	"https://github.com/GemTalk/Rowan/issues/799"

	| packageTools projectName packageNames className packageName1 packageName2 theClass audit oldMethod newMethod loadedMethod componentName definedProject |
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 . packageName2 . }.
	className := 'AuditClass'.
	componentName := 'Core'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addTopLevelComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		packageConvention: 'Rowan';
		yourself.
	definedProject resolve load.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	oldMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'cat' 
		packageName: packageName2.
	loadedMethod :=  Rowan image loadedMethodForMethod: oldMethod.
	newMethod := theClass
		rwCompileMethod: 'foo "" ^''foo'''
		category: 'cat'.
"brain surgery --- transplant oldMethod in loadedMethod, but newMethod is in method dictionary"
	loadedMethod handle: oldMethod.

	audit := Rowan projectTools audit auditForProjectNamed:  projectName.
	self assert: audit size equals: 1
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testNonIdenticalMethods_2 [
	"Three 'method is not identical' audit errors:
		RwClsAuditTool>>_auditSelector:forBehavior:loadedClass:
		RwClsAuditTool>>_auditSelector:forBehavior:loadedClass:
		RwClsExtensionAuditTool>>auditLoadedClassExtension:
	"

	"https://github.com/GemTalk/Rowan/issues/799"

	| packageTools projectName packageNames className packageName1 packageName2 theClass audit oldMethod newMethod loadedMethod componentName definedProject |
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 . packageName2 . }.
	className := 'AuditClass'.
	componentName := 'Core'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addTopLevelComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		packageConvention: 'RowanHybrid';
		yourself.
	definedProject resolve load.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	oldMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*', packageName2 asLowercase.
	loadedMethod :=  Rowan image loadedMethodForMethod: oldMethod.
	newMethod := theClass
		rwCompileMethod: 'foo "" ^''foo'''
		category: '*', packageName2 asLowercase.
"brain surgery --- transplant oldMethod in loadedMethod, but newMethod is in method dictionary"
	loadedMethod handle: oldMethod.

	audit := Rowan projectTools audit auditForProjectNamed:  projectName.
	self assert: audit size equals: 1
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testNotification [

| packageTools projectName packageNames className packageName theClass fooMethod |
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: "'*' , "packageName asLowercase.
	 
	fooMethod := theClass class
		compileMethod: 'new ^super new'
		dictionaries: #()
		category: 'Instance Creation'.

	theClass compileMissingAccessingMethods. "this should add: #bar #bar:"


	[Rowan projectTools audit auditForProjectNamed:  'AuditProject'] on: Notification do: [:ex | self assert: (ex description matchPattern: {$* . 'Missing loaded method' . $*})].

]
