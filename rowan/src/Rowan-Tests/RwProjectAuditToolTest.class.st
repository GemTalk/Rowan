Class {
	#name : 'RwProjectAuditToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'other' }
RwProjectAuditToolTest class >> comment [

	^'testing audit api'
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testClassExtension [

| packageTools projectName packageNames className packageName1 packageName2 theClass loadedClass loadedClassExtension fooMethod x y|
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

   theClass rwCompileMethod: 'bar ^bar'
	     category: 'Accessing' "'*'  packageName1 asLowercase".

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.

	fooMethod := theClass
		compileMethod: 'bar: aBar bar := aBar'
		dictionaries:  #() 
		category: '*' , packageName2 asLowercase.


"	((Rowan image loadedPackageNamed: 'Audit-Core') loadedClassForClass: (Rowan globalsAt: className asSymbol) value 
		ifAbsent: [nil error: 'expected a loaded class']) 
			loadedInstanceMethodsDo: [:anLMethod | (anLMethod name isEquivalent: 'bar') ifTrue: [self error: 'bar was compiled without Rowan']] 
			loadedClassMethodsDo: [:cMethod | ].
"
x := RwClsAuditTool new auditLoadedClass: (loadedClass := Rowan image loadedClassForClass: theClass ifAbsent: [nil]).
y := RwClsExtensionAuditTool new auditLoadedClassExtenstion: (loadedClassExtension := (Rowan image loadedClassExtensionsForClass: theClass) _at: 1).

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName2) at: className) size = 1
]

{ #category : 'tests' }
RwProjectAuditToolTest >> testClassVars [
	| projectName packageNames className packageName classDefinition browserTool testClass testClassB testSymDict x |
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
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a Simple class with various vars'
		pools: #()
		type: 'normal'.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') isEmpty.
	
	DateTime subclass: className
	instVarNames: #( )
	classVars: #( )
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: (System myUserProfile resolveSymbol: self _symbolDictionaryName) value
	newVersionOf: testClass
	description: 'Non rowan class'
	options: #().

	testClassB := Rowan globalNamed: className.
	self deny: testClassB == testClass.
	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName) at: className) size = 4 description: 'expected 4 failures superclass instvars classvars and comment'
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


	theClass compileMissingAccessingMethods. "this should add: #bar #bar:"


	((Rowan image loadedPackageNamed: 'Audit-Core') loadedClassForClass: (GsSession currentSession resolveSymbol: className asSymbol) value ifAbsent: [nil error: 'expected a class']) 
			loadedInstanceMethodsDo: [:anLMethod | (anLMethod name isEquivalent: 'bar') ifTrue: [self error: 'bar was compiled without Rowan']] 
			loadedClassMethodsDo: [:cMethod | "do nothing"].

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName) at: className) size = 2
		


"	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass class inProjectNamed: projectName.

	self _writeProjectNamed: projectName.
"
]
