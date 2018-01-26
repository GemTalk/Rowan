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

{ #category : 'private' }
RwSymbolDictionaryTest >> _newOrExistingPackageSymbolDictionaryNamed: dictName [

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^ symbolList
		detect: [ :each | (each at: symbolName ifAbsent: [ nil ]) == each ]
		ifNone: [ 
			| newDict |
			newDict := RwGsPackageSymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			symbolList addLast: newDict.
			session userProfile symbolList addLast: newDict.
			newDict ]
]

{ #category : 'support' }
RwSymbolDictionaryTest >> definitionsOfLoadedPackages: packageNames [

	"symbolDictionary based package definitions ... for now"

	| symbolDict loadedPackages |
	symbolDict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	loadedPackages := packageNames
		collect: [ :packageName | symbolDict packageRegistry at: packageName ifAbsent: [  ] ].
	^ (RwPackageSet withAll: (loadedPackages select: [ :each | each notNil ]))
		asDefinition
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassAdditionPatch [

	"RwGsClassAdditionPatch>>installClassInSystem"

	"add a packaged class to a RwPackageSymbolDictionary"

	| dict class assoc packageName className superclassName testClass expectedPackageSet loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
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
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
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
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.

	dict deleteClassFromPackage: class.

	testClass := System myUserProfile symbolList objectNamed: className.
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
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.

	newClass := self _applyBenignPropertiesTo: class.
	dict updateClassProperties: newClass.

	testClass := System myUserProfile symbolList objectNamed: className.
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
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.

	newClass := self _applyClassVariableChangeTo: class.
	dict updateClassProperties: newClass.

	testClass := System myUserProfile symbolList objectNamed: className.
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
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.

	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.

	newClass := self _applyClassNewVersionChangesTo: class.
	dict addNewClassVersionToAssociation: newClass.
	superclassName := 'Association'.

	testClass := System myUserProfile symbolList objectNamed: className.
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
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := 'accessing'.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addNewCompiledMethod: compiledMethod
		into: class
		protocol: methodProtocol
		toPackageNamed: packageName.

	testInstance := testClass new.
	self assert: testInstance foo = 'foo'.

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

	| dict class assoc packageName compiledMethod className testClass testInstance |
	className := 'TestSymbolDictClass'.
	class := self _createClassNamed: className superclassName: 'Object'.
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	compiledMethod := self
		_compileMethodIn: class
		source: 'foo ^ ''foo'''
		dictionaries: GsCurrentSession currentSession symbolList.

	dict
		addNewCompiledMethod: compiledMethod
		into: class
		protocol: 'accessing'
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.

	dict deleteCompiledMethod: compiledMethod from: class.

	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodPropertiesPatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then change it's properties."

	| dict class assoc packageName compiledMethod className testClass testInstance protocol x |
	className := 'TestSymbolDictClass'.
	class := self _createClassNamed: className superclassName: 'Object'.
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	compiledMethod := self
		_compileMethodIn: class
		source: 'foo ^ ''foo'''
		dictionaries: GsCurrentSession currentSession symbolList.

	protocol := 'accessing'.
	dict
		addNewCompiledMethod: compiledMethod
		into: class
		protocol: protocol
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.
	self assert: (x := testClass categoryOfSelector: #'foo') = protocol asSymbol.

	protocol := 'the accessing'.
	dict moveCompiledMethod: compiledMethod toProtocol: protocol.

	self assert: testInstance foo = 'foo'.
	self assert: (testClass categoryOfSelector: #'foo') = protocol asSymbol
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testMethodSourcePatch [

	"RwGsMethodSourcePatch>>installSourcePatch"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then change it's source."

	| dict class assoc packageName compiledMethod className testClass testInstance protocol x newCompiledMethod |
	className := 'TestSymbolDictClass'.
	class := self _createClassNamed: className superclassName: 'Object'.
	dict := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packageName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packageName.

	testClass := System myUserProfile symbolList objectNamed: className.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	compiledMethod := self
		_compileMethodIn: class
		source: 'foo ^ ''foo'''
		dictionaries: GsCurrentSession currentSession symbolList.

	protocol := 'accessing'.
	dict
		addNewCompiledMethod: compiledMethod
		into: class
		protocol: protocol
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.
	self assert: (x := testClass categoryOfSelector: #'foo') = protocol asSymbol.

	newCompiledMethod := self
		_compileMethodIn: class
		source: 'foo ^ ''bar'''
		dictionaries: GsCurrentSession currentSession symbolList.

	dict addRecompiledMethod: newCompiledMethod.

	self assert: testInstance foo = 'bar'.
	self assert: (x := testClass categoryOfSelector: #'foo') = protocol asSymbol
]
