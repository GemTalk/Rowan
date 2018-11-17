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
								ar addAll: {aSelector  -> 'Missing loaded classmethod ' }.
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
								ar addAll: { aLoadedMethod name  -> 'Missing compiled classmethod ' }.
								GsFile gciLogServer: 'Missing compiled method ', loadedClass name, ' class>>', aLoadedMethod name ] ] ].
		ar ].

]

{ #category : 'tests' }
RwProjectAuditToolTest >> _auditLoadedClassExtensionBlock [

^[:aLoadedClassExtension |
		| ar |
		ar := {}.
		(Rowan globalNamed: aLoadedClassExtension name) 
			ifNil: [ 
				ar addAll: { 'Class does not exists '-> aLoadedClassExtension name }.
				GsFile gciLogServer: 'Class does not exist ', aLoadedClassExtension name ]
			ifNotNil: [ :aBehavior |
				| selectors extensionCategoryName |
				extensionCategoryName := '*', aLoadedClassExtension loadedPackage name asLowercase.

				((aBehavior _baseCategorys: 0) keys
						detect: [:each | each equalsNoCase: extensionCategoryName ]
						ifNone: [ ])
					ifNotNil: [:categoryName |
						selectors := aBehavior selectorsIn: categoryName.
						(selectors reject: [:e | 
							aLoadedClassExtension includesSelector: e isMeta: false]) 
								do: [:aSelector |
									ar addAll: { aSelector -> 'Missing loaded extension method' ->  aSelector }.
									GsFile gciLogServer: 'Missing loaded extension method ', aLoadedClassExtension name, '>>', aSelector ].
						selectors do: [:sel |
							| cMeth lMeth |
							(cMeth := aBehavior compiledMethodAt: sel otherwise: nil) == (lMeth := (aLoadedClassExtension loadedInstanceMethods at: sel) handle)
								ifFalse: [
									ar addAll: { sel -> 'compiled Method is not identical to loaded Method' }.
									GsFile gciLogServer: 
										'Compiled Method (oop: ', 
										cMeth asOop printString, 
										') is not identical to loaded Method (oop: ',
									  lMeth asOop printString, 
										') ', 
										aLoadedClassExtension name, 
										'>>', 
										sel ] ] ]
					ifNil: [ 
						aLoadedClassExtension loadedInstanceMethods isEmpty not
							ifTrue: [
								ar addAll: 
									{('expected methods in the category ', extensionCategoryName printString, ' are missing') -> aLoadedClassExtension name }.
								GsFile gciLogServer: 'expected methods in category ', extensionCategoryName printString, 'for class ', aLoadedClassExtension name printString ] 
				].

				((aBehavior class _baseCategorys: 0) keys
						detect: [:each | each equalsNoCase: extensionCategoryName ]
						ifNone: [ ])
					ifNotNil: [:categoryName |
						selectors := aBehavior class selectorsIn: categoryName.
						(selectors reject: [:e | aLoadedClassExtension includesSelector: e isMeta: true]) do: [:aSelector |
								ar addAll: {aSelector -> 'Missing loaded extensions classmethod: ' }.
								GsFile gciLogServer: 'Missing loaded extension method ', aLoadedClassExtension name, ' class>>', aSelector ].
						selectors do: [:sel |
							| cMeth lMeth |
							(cMeth := aBehavior class compiledMethodAt: sel otherwise: nil) == (lMeth := (aLoadedClassExtension loadedClassMethods at: sel) handle)
								ifFalse: [
									ar addAll: { sel -> 'class compiled Method is not identical to loaded Method' }.
									GsFile gciLogServer: 
										'Compiled Method (oop: ', 
										cMeth asOop printString, 
										') is not identical to loaded Method (oop: ',
									  lMeth asOop printString, 
										') ', 
										aLoadedClassExtension name, 
										' class>>', 
										sel ] ]  ]
					ifNil: [
						aLoadedClassExtension loadedClassMethods isEmpty not
							ifTrue: [ 
								ar addAll: {('expected class methods in the category ', extensionCategoryName printString, ' are missing') -> aLoadedClassExtension name }.
								GsFile gciLogServer: 'expected methods in category ', extensionCategoryName printString, 'for class ', aLoadedClassExtension name printString, ' class' ] 
				].

				"look for methods removed outside of Rowan API"
				aLoadedClassExtension 
					loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
						(aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: {aLoadedMethod name -> 'Missing compiled method: ' }.
								GsFile gciLogServer: 'Missing compiled  method ', loadedClass name, '>>', aLoadedMethod name ] ]
					loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
						(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: { aLoadedMethod name -> 'Missing compiled class method: ' }.
								GsFile gciLogServer: 'Missing compiled  class method ', loadedClass name, ' class>>', aLoadedMethod name ] ] ].
		ar ].

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
		dictionaries:  #() 
		category: '*' , packageName1 asLowercase .

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 2.
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
	self assert: ((x at: packageName) at: className) size = 1
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
