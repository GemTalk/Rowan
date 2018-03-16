Class {
	#name : 'RwHybridBrowserToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCreation [

	| byteClass indexableClass normalClass projectName packageNames packageName1 packageName2 symbolDict  |
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
	symbolDict := Rowan image objectNamed: self _symbolDictionaryName1.
	byteClass := Object
		rwByteSubclass: 'SimpleHybridByte1'
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName1
		options: #().
	self assert: byteClass rowanPackageName = packageName1.

	indexableClass := Object
		rwIndexableSubclass: 'SimpleHybridIndexable1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName1
		options: #().
	self assert: indexableClass rowanPackageName = packageName1.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.


]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackage [

	| normalClass projectName packageNames packageName1 packageName2 symbolDict movedNormalClass |
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
	symbolDict := Rowan image objectNamed: self _symbolDictionaryName1.
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName2
		options: #().
	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageWithClassModifications [

	| normalClass projectName packageNames packageName1 packageName2 symbolDict movedNormalClass |
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
	symbolDict := Rowan image objectNamed: self _symbolDictionaryName1.
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDict
		category: packageName2
		options: #().
	self assert: movedNormalClass ~= normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2
]
