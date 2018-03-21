Class {
	#name : 'RwHybridBrowserToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCreation [

	| byteClass indexableClass normalClass projectName packageNames packageName1 packageName2 |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	byteClass := Object
		rwByteSubclass: 'SimpleHybridByte1'
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: byteClass rowanPackageName = packageName1.

	indexableClass := Object
		rwIndexableSubclass: 'SimpleHybridIndexable1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: indexableClass rowanPackageName = packageName1.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethod [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass fooMethod barMethod normalInstance |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'
		packageName: packageName2.

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackage [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageWithClassModifications [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	self assert: movedNormalClass ~= normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesA [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass superClass == class.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan image objectNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsA [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName1 asLowercase.
	class class
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	subclass
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.
	subclass class
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.

	self assert: subclass superClass == class.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan image objectNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: newSubclassVersion foo = 'foo'.
	self assert: newSubclassVersion foo1 = 'foo'.
	self assert: newSubclassVersion new foo = 'foo'.
	self assert: newSubclassVersion new foo1 = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsB [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	class class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	subclass rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.
	subclass class rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.

	self assert: subclass superClass == class.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan image objectNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: newSubclassVersion foo = 'foo'.
	self assert: newSubclassVersion foo1 = 'foo'.
	self assert: newSubclassVersion new foo = 'foo'.
	self assert: newSubclassVersion new foo1 = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesB [

	| class subclass1 subclass2 subclass3 subclass4 projectName packageNames packageName1 newClassVersion newSubclassVersion1 newSubclassVersion2 newSubclassVersion3 newSubclassVersion4 |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	subclass1 := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass1 superClass == class.

	subclass2 := subclass1
		rwSubclass: 'SimpleHybridSubclass2'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass2 superClass == subclass1.

	subclass3 := subclass2
		rwSubclass: 'SimpleHybridSubclass3'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass3 superClass == subclass2.

	subclass4 := subclass3
		rwSubclass: 'SimpleHybridSubclass4'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass4 superClass == subclass3.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion1 := Rowan image objectNamed: subclass1 name.
	self assert: newSubclassVersion1 ~~ subclass1.
	self assert: newSubclassVersion1 superClass == newClassVersion.

	newSubclassVersion2 := Rowan image objectNamed: subclass2 name.
	self assert: newSubclassVersion2 ~~ subclass2.
	self assert: newSubclassVersion2 superClass == newSubclassVersion1.

	newSubclassVersion3 := Rowan image objectNamed: subclass3 name.
	self assert: newSubclassVersion3 ~~ subclass3.
	self assert: newSubclassVersion3 superClass == newSubclassVersion2.

	newSubclassVersion4 := Rowan image objectNamed: subclass4 name.
	self assert: newSubclassVersion4 ~~ subclass4.
	self assert: newSubclassVersion4 superClass == newSubclassVersion3
]
