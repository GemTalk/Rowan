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

   theClass rwCompileMethod: 'bar ^bar'
	     category: 'Accessing' "'*'  packageName1 asLowercase".

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.

	fooMethod := theClass
		compileMethod: 'foo ^2'
		dictionaries:  #() 
		category: '*' , packageName1 asLowercase .

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 2.
	self assert: ( y := x at: packageName1) notNil; 
		assert: y size = 1;
		assert: (y at: className) size = 1;
		assert: (y := x at: packageName2 ) notNil;
		assert: y size = 1;
		assert: (y at: className) size = 2

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
"lets mess up the class extension"


    fooMethod := theClass
		compileMethod: 'bar: aBar bar := aBar'
		dictionaries:  #() 
		category: '*' , packageName2 asLowercase.

	 fooMethod := theClass class
		compileMethod: 'new2 ^self new'
		dictionaries: #()
		category: '*' , packageName2 asLowercase.

	fooMethod := theClass class
		compileMethod: 'new3 ^self new initialize'
		dictionaries: #()
		category: '*' , packageName3 asLowercase.

	 theClass class compileMethod: 'new ^super new '
		dictionaries: #()
		category: '*Instance creation'. "non existing extension/package"
x := Rowan projectTools audit auditForProjectNamed:  'AuditProject'.


	self 
	  assert: x size = 3;
	  assert: ((y := x at: packageName1) at: className) size = 1;
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
