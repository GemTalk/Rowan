Class {
	#name : 'RwSymbolDictionaryTest',
	#superclass : 'TestCase',
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
		description: ''
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
		instVarNames: #('ivar1')
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
		classVars: #('ClassVar1')
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
RwSymbolDictionaryTest >> _newOrExistingPackageSymbolDictionaryNamed: dictName [
	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^ symbolList
		detect: [ :each | (each at: symbolName ifAbsent: [ nil ]) == each ]
		ifNone: [ 
			| newDict |
			newDict := RwPackageSymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			symbolList addLast: newDict.
			session userProfile symbolList addLast: newDict.
			newDict ]
]

{ #category : 'running' }
RwSymbolDictionaryTest >> setUp [

	"Remove the CypressTestData symbol dictionary, if present --- gets persistent and session symbol list"

	| user symList dict index |
	user := System myUserProfile.
	symList := user symbolList.

	dict := symList objectNamed: #'CypressTestData'.
	dict
		ifNotNil: [ 
			index := symList indexOf: dict.
			index > 0
				ifTrue: [ user removeDictionaryAt: index ] ]
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassAdditionPatch [

	"RwGsClassAdditionPatch>>installClassInSystem"

	"add a packaged class to a RwPackageSymbolDictionary"

	| dict class assoc packagName |
	class := self _createClassNamed: 'TestSymbolDictClass' superclassName: 'Object'.
	dict := RwPackageSymbolDictionary newNamed: #'CypressTestData'.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packagName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packagName
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassDeletionPatch [

	"RwGsClassDeletionPatch>>deleteClassFromSystem"

	"add a packaged class to a RwPackageSymbolDictionary, then delete it"

	| dict class assoc packagName |
	class := self _createClassNamed: 'TestSymbolDictClass' superclassName: 'Object'.
	dict := RwPackageSymbolDictionary newNamed: #'CypressTestData'.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: class.
	packagName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packagName.

	dict deleteClassFromPackage: class
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassPropertiesPatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then update properties (non-class version/class variable properties)"

	| dict existingClass assoc packagName newClass |
	existingClass := self
		_createClassNamed: 'TestSymbolDictClass'
		superclassName: 'Object'.
	dict := RwPackageSymbolDictionary newNamed: #'CypressTestData'.
	assoc := SymbolAssociation
		newWithKey: existingClass name asSymbol
		value: existingClass.
	packagName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packagName.

	newClass := self _applyBenignPropertiesTo: existingClass.
	dict updateClassProperties: newClass
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassVariableChangePatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then update class variable properties"

	| dict existingClass assoc packagName newClass |
	existingClass := self
		_createClassNamed: 'TestSymbolDictClass'
		superclassName: 'Object'.
	dict := RwPackageSymbolDictionary newNamed: #'CypressTestData'.
	assoc := SymbolAssociation
		newWithKey: existingClass name asSymbol
		value: existingClass.
	packagName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packagName.

	newClass := self _applyClassVariableChangeTo: existingClass.
	dict updateClassProperties: newClass
]

{ #category : 'tests' }
RwSymbolDictionaryTest >> testClassVersioningPatch [

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then apply some new version inducing class changes"

	| dict existingClass assoc packagName newClass |
	existingClass := self
		_createClassNamed: 'TestSymbolDictClass'
		superclassName: 'Object'.
	dict := self _newOrExistingPackageSymbolDictionaryNamed: #'CypressTestData'.
	assoc := SymbolAssociation
		newWithKey: existingClass name asSymbol
		value: existingClass.
	packagName := 'TestSymbolDictPackage'.
	dict addClassAssociation: assoc toPackageNamed: packagName.

	newClass := self _applyClassNewVersionChangesTo: existingClass.
	dict addNewClassVersionToAssociation: newClass
]
