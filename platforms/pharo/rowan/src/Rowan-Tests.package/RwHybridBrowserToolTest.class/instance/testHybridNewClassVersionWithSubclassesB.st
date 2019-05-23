tests
testHybridNewClassVersionWithSubclassesB

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
	newSubclassVersion1 := Rowan globalNamed: subclass1 name.
	self assert: newSubclassVersion1 ~~ subclass1.
	self assert: newSubclassVersion1 superClass == newClassVersion.

	newSubclassVersion2 := Rowan globalNamed: subclass2 name.
	self assert: newSubclassVersion2 ~~ subclass2.
	self assert: newSubclassVersion2 superClass == newSubclassVersion1.

	newSubclassVersion3 := Rowan globalNamed: subclass3 name.
	self assert: newSubclassVersion3 ~~ subclass3.
	self assert: newSubclassVersion3 superClass == newSubclassVersion2.

	newSubclassVersion4 := Rowan globalNamed: subclass4 name.
	self assert: newSubclassVersion4 ~~ subclass4.
	self assert: newSubclassVersion4 superClass == newSubclassVersion3