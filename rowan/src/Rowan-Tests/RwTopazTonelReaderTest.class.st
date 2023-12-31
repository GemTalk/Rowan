"
No class-specific documentation for RwTonelReaderTest, hierarchy is:
Object
  TestAsserter
    TestCase( testSelector)
      RwTonelReaderTest

"
Class {
	#name : 'RwTopazTonelReaderTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwTopazTonelReaderTest class >> testCompileMethod [
	| symbolDictionary className class tonelMethodString |
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	className := 'RwTonelReaderTestClass'.
	class := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

"compile method"
	tonelMethodString := '{ #category : ''private'' }
' , className
		,
			' >> foo [
	^1
]
'.
	RwTopazTonelReader topazCompileTonelMethod: tonelMethodString.

"validate"
	self assert: (class new perform: #foo) = 1.
	self assert: (class categoryOfSelector: #foo) = 'private'
]

{ #category : 'tests' }
RwTopazTonelReaderTest >> testCompileMethod_01 [
	"category plus method block"

	| symbolDictionary className class tonelMethodString x |
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	className := 'RwTonelReaderTestClass'.
	class := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

"compile method"
	tonelMethodString := '{ #category : ''private'' }
' , className
		,
			' >> foo [
	^1
]
'.
	self assert: (RwTopazTonelReader topazCompileTonelMethod: tonelMethodString) isNil.

"validate"
	self assert: (class new perform: #foo) = 1.
	self assert: (x := class categoryOfSelector: #foo) = #'private'
]

{ #category : 'tests' }
RwTopazTonelReaderTest >> testCompileMethod_02 [
	"just method block"

	| symbolDictionary className class tonelMethodString |
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	className := 'RwTonelReaderTestClass'.
	class := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

"compile method no category defined"
	tonelMethodString := className
		,
			' >> foo [
	^1
]
'.
	self should: [ RwTopazTonelReader topazCompileTonelMethod: tonelMethodString ] raise: Error
]

{ #category : 'tests' }
RwTopazTonelReaderTest >> testCompileMethod_03 [
	"category plus method block and compiler warning"

	| symbolDictionary className class tonelMethodString warning |
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	className := 'RwTonelReaderTestClass'.
	class := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

"compile method"
	tonelMethodString := '{ #category : ''private'' }
' , className
		,
			' >> foo [
	| unusedMethodTemp |
	^1
]
'.
	self assert: (warning := RwTopazTonelReader topazCompileTonelMethod: tonelMethodString) = 'WARNING: unused method temp ''unusedMethodTemp''
'.

"validate"
	self assert: (class new perform: #foo) = 1.
	self assert: (class categoryOfSelector: #foo) = #'private'
]

{ #category : 'tests' }
RwTopazTonelReaderTest >> testCompileMethodsInFile [
	| symbolDictionary className class rowanProject tonelFileReference |
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	className := 'TestClass1'.
	class := Object
		subclass: className
		instVarNames: #(instVar1)
		classVars: #(ClassVar1)
		classInstVars: #(classInstVar1 classInstVar2)
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

"compile stream"
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	tonelFileReference := rowanProject repositoryRoot / 'test'/ 'testRepositories' /'tonel'/'CombinedTestPackage1'/'TestClass1.class.st'.

	RwTopazTonelReader topazReadTonelFile: tonelFileReference pathString.

"validate"
	self assert: (class perform: #testClassMethod1) isNil.
	self assert: (class class categoryOfSelector: #testClassMethod1) = #'accessing'.
	self assert: (class perform: #testClassMethod2) isNil.
	self assert: (class class categoryOfSelector: #testClassMethod2) = #'accessing'.
	self assert: (class perform: #testClassMethod3).
	self assert: (class class categoryOfSelector: #testClassMethod3) = #'accessing'.

	self assert: (class new perform: #testInstanceMethod1) isNil.
	self assert: (class categoryOfSelector: #testInstanceMethod1) = #'accessing'.
	self assert: (class new perform: #testInstanceMethod1) isNil.
	self assert: (class categoryOfSelector: #testInstanceMethod2) = #'accessing'.
]
