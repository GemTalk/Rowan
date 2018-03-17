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
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclasses [

	| class subclass projectName packageNames packageName1 newClassVersion |
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
	self assert: subclass superClass == newClassVersion
]
