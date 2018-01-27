Class {
	#name : 'RwSymbolDictionaryTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwSymbolDictionaryTest >> _applyBenignPropertiesTo: existingClass [

	"benign properties are properties that do not cause a new class version or modify class variables"

	| newClass |
	newClass := existingClass superclass
		subclass: existingClass name
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: existingClass
		description: self _classComment
		options: #().
	self assert: newClass == existingClass.	"not supposed to create a new class version"
	^ newClass
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _applyClassNewVersionChangesTo: existingClass [

	"benign properties are properties that do not cause a new class version or modify class variables"

	| newClass superclass |
	superclass := System myUserProfile objectNamed: 'Association'.
	newClass := superclass
		subclass: existingClass name
		instVarNames: self _instVars
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: existingClass
		description: ''
		options: #().
	self assert: newClass ~~ existingClass.	"supposed to create a new class version"
	^ newClass
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _applyClassVariableChangeTo: existingClass [

	"benign properties are properties that do not cause a new class version or modify class variables"

	| newClass |
	newClass := existingClass superclass
		subclass: existingClass name
		instVarNames: #()
		classVars: self _classVars
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: existingClass
		description: ''
		options: #().
	self assert: newClass == existingClass.	"not supposed to create a new class version"
	^ newClass
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _classComment [

	^ 'a comment'
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _classVars [

	^ #('ClassVar1')
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _compileMethodIn: behavior source: sourceString dictionaries: symbolList [

	^ behavior
		compileMethod: sourceString
		dictionaries: symbolList
		category: nil
		intoMethodDict: false
		intoCategories: nil
		intoPragmas: nil
		environmentId: 0
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _createClassNamed: className superclassName: superclassName [

	| superclass |
	superclass := System myUserProfile objectNamed: superclassName.
	^ superclass
		subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: nil
		description: ''
		options: #()
]

{ #category : 'private' }
RwSymbolDictionaryTest >> _instVars [

	^ #('ivar1')
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassAdditionPatch [

	"RwGsClassAdditionPatch>>installClassInSystem"

	"add a packaged class to a RwPackageSymbolDictionary"

	| dict class assoc packageName className superclassName testClass expectedPackageSet loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassAdditionPatchWithCategory [

	"RwGsClassAdditionPatch>>installClassInSystem"

	"add a packaged class to a RwPackageSymbolDictionary"

	| dict class assoc packageName className superclassName testClass expectedPackageSet loadedPackageSet diff category |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	category := '*NotAPackageName'.
	class := self _createClassNamed: className superclassName: superclassName.
	class category: category.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		category: category.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassDeletionPatch [

	"RwGsClassDeletionPatch>>deleteClassFromSystem"

	"add a packaged class to a RwPackageSymbolDictionary, then delete it"

	| dict class assoc packageName className testClass expectedPackageSet loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	class := self _createClassNamed: className superclassName: 'Object'.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	dict deleteClassFromPackage: class.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass isNil.

	expectedPackageSet := self packageSetDefinition: packageName.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassPropertiesPatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then update properties (non-class version/class variable properties)"

	| dict class assoc packageName compiledMethod className testClass newClass expectedPackageSet superclassName loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: 'Object'.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	newClass := self _applyBenignPropertiesTo: class.
	dict updateClassProperties: newClass.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	self assert: testClass comment = self _classComment.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		comment: self _classComment.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassVariableChangePatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then update class variable properties"

	| dict class assoc packageName className testClass newClass expectedPackageSet superclassName loadedPackageSet diff x y |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	newClass := self _applyClassVariableChangeTo: class.
	dict updateClassProperties: newClass.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	self
		assert:
			(x := self _classVars)
				=
					(y := testClass _classVars keys asArray collect: [ :each | each asString ]).

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		classVars: self _classVars.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassVersioningPatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then apply some new version inducing class changes"

	| dict class assoc packageName className testClass newClass expectedPackageSet loadedPackageSet diff superclassName x y |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	newClass := self _applyClassNewVersionChangesTo: class.
	dict addNewClassVersionToAssociation: newClass.
	superclassName := 'Association'.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass ~~ class.
	self assert: testClass == newClass.
	self
		assert:
			(x := self _instVars)
				= (y := testClass instVarNames collect: [ :each | each asString ]).

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instVars: self _instVars.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodAdditionPatch [

	"RwGsMethodAdditionPatch>>installMethod"

	"add a packaged class to a RwPackageSymbolDictionary, then add a new method."

	| dict class assoc packageName compiledMethod className testClass testInstance expectedPackageSet superclassName loadedPackageSet diff methodSource methodProtocol methodSelector |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self assert: testClass == class.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := RwGsPackageSymbolDictionary defaultProtocolString.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict addRecompiledMethod: compiledMethod.	"this should work, ends up using the package of the class"

	self assert: testInstance foo = 'foo'.
	self
		assert:
			(testClass categoryOfSelector: methodSelector) = methodProtocol asSymbol.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instanceMethods:
			(self
				methodDefsFromSpec:
					{{methodSelector.
					methodProtocol.
					methodSource}})
		classMethods: Dictionary new.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodDeletionPatch [

	"RwGsMethodDeletionPatch>>deleteMethodNewClasses:andExistingClasses:"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then delete it."

	| dict class assoc packageName compiledMethod className testClass testInstance expectedPackageSet superclassName loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	compiledMethod := self
		_compileMethodIn: class
		source: 'foo ^ ''foo'''
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addNewCompiledMethod: compiledMethod
		for: class
		protocol: 'accessing'
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.

	dict deleteCompiledMethod: compiledMethod from: class.

	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instanceMethods: Dictionary new
		classMethods: Dictionary new.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodExtensionPatchInGlobalsExtension [

	"extend a class that is NOT owned by user, i.e., the class is in Globals and the user is DataCurator"

	
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodExtensionPatchInSymbolDictionaryExtension [

	"RwGsClassExtensionPatch>>installClassExtensionInSystem"

	"add a packaged class to a RwPackageSymbolDictionary, then add method to class using different package."

	| dict class assoc packageName1 packageName2 compiledMethod className testClass testInstance expectedPackageSet expectedPackage1 expectedPackage2 superclassName loadedPackageSet diff methodSource methodProtocol methodSelector |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName1 := 'TestSymbolDictPackage'.
	packageName2 := 'TestSymbolDictExtensionPackage'.
	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName1.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self assert: testClass == class.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	expectedPackage1 := self
		packageDefinition: packageName1
		classNamed: className
		super: superclassName.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := 'accessing'.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addExtensionCompiledMethod: compiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName2.

	self assert: testInstance foo = 'foo'.
	self
		assert:
			(testClass categoryOfSelector: methodSelector) = methodProtocol asSymbol.

	expectedPackage2 := self
		packageDefinition: packageName2
		extendClassNamed: className
		instanceMethods:
			(self
				methodDefsFromSpec:
					{{methodSelector.
					methodProtocol.
					methodSource}})
		classMethods: Dictionary new.

	expectedPackageSet := (RwPackageSetDefinition new
		addPackage: expectedPackage1;
		addPackage: expectedPackage2;
		yourself) asDefinition.
	loadedPackageSet := self
		definitionsOfLoadedPackages:
			{packageName1.
			packageName2}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodExtensionPatchInUserExtension [

	"extend a class that is owned by user, but not in the same symbol dictionary as package"

	
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodPropertiesPatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then change it's properties."

	| dict class assoc packageName compiledMethod className testClass testInstance x superclassName expectedPackageSet methodSource methodProtocol methodSelector loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := 'accessing'.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addNewCompiledMethod: compiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	methodProtocol := 'the accessing'.
	dict moveCompiledMethod: compiledMethod toProtocol: methodProtocol.

	self assert: testInstance foo = 'foo'.
	self assert: (testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instanceMethods:
			(self
				methodDefsFromSpec:
					{{methodSelector.
					methodProtocol.
					methodSource}})
		classMethods: Dictionary new.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodSourcePatch [

	"RwGsMethodSourcePatch>>installSourcePatch"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then change it's source."

	| dict class assoc packageName compiledMethod className testClass testInstance x newCompiledMethod superclassName methodSource methodProtocol methodSelector expectedPackageSet loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc forClass: class toPackageNamed: packageName.

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := 'accessing'.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addNewCompiledMethod: compiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	methodSource := 'foo ^ ''bar'''.
	newCompiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict addRecompiledMethod: newCompiledMethod.

	self assert: testInstance foo = 'bar'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	methodSource := 'foo ^ ''who'''.
	methodProtocol := 'the accessing'.
	newCompiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addNewCompiledMethod: newCompiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName.	"account for using wrong method to update a method --- perhaps the selector is incorrect?"

	self assert: testInstance foo = 'who'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instanceMethods:
			(self
				methodDefsFromSpec:
					{{methodSelector.
					methodProtocol.
					methodSource}})
		classMethods: Dictionary new.
	loadedPackageSet := self definitionsOfLoadedPackages: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
]
