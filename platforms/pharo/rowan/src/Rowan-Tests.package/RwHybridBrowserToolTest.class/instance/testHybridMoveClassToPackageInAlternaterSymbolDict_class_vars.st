tests
testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	packageNameMap := Dictionary new
		at: packageName1 put: self _symbolDictionaryName1;
		at: packageName2 put: self _symbolDictionaryName2;
		yourself.

	self
		_loadPackageMappedProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: self _symbolDictionaryName1.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category= packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
