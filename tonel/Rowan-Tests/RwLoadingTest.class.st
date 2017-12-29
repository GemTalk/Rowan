"
No class-specific documentation for CypLoadingTest, hierarchy is: 
Object
  TestAsserter
    TestCase( testSelector)
      CypLoadingTest

"
Class {
	#name : 'RwLoadingTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'running' }
RwLoadingTest class >> debug [

	^self allTestSelectors do: [:selector | self debug: selector]
]

{ #category : 'running' }
RwLoadingTest class >> run [

	^self suite run
]

{ #category : 'support - simple definitions' }
RwLoadingTest >> accessorMethodsForSimpleClassNamed: name [

	| result |
	result := Dictionary new.
	
	{'iv1In' , name.
	'iv2In' , name} do: 
				[:ivName |
				result
					at: ivName asSymbol
						put: (RwMethodDefinition
								withProperties: (Dictionary with: 'selector' -> ivName asSymbol
										with: 'protocol' -> 'accessing')
								source: ivName , ' ^' , ivName);
					at: (ivName , ':') asSymbol
						put: (RwMethodDefinition
								withProperties: (Dictionary with: 'selector' -> (ivName , ':') asSymbol
										with: 'protocol' -> 'accessing')
								source: ivName , ': anObject ' , ivName , ' := anObject')].
	^result
]

{ #category : 'support' }
RwLoadingTest >> applyDiffToImage: aPackageSetModification [

	Rowan image applyModification: aPackageSetModification
]

{ #category : 'support' }
RwLoadingTest >> assertAbsenceOfPackage: packageName [

	| loadedPackage |
	loadedPackage := Rowan loadedPackageNamed: packageName ifAbsent: [nil].
	self assert: loadedPackage equals: nil
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> basicClassMethodsForClass: className [
  ^ self
    basicMethodDefsFromSpec:
      {{#'classMeth'.
      'accessing'.
      ('classMeth ^''classMeth for class ''' , className printString)}}
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> basicInstanceMethodsForClass: className [
  ^ self
    basicMethodDefsFromSpec:
      {{#'instanceMeth'.
      'accessing'.
      ('instanceMeth ^''instanceMeth for class ''' , className printString)}}
]

{ #category : 'support' }
RwLoadingTest >> basicMethodDefsFromSpec: specArray [
  | dict |
  dict := Dictionary new.
  specArray
    do: [ :spec | 
      | selector protocol source |
      selector := (spec at: 1) asSymbol.
      protocol := (spec at: 2) asString.
      source := (spec at: 3) asString.
      dict
        at: selector
        put:
          (RwMethodDefinition
            withProperties:
              (Dictionary with: 'selector' -> selector with: 'protocol' -> protocol)
            source: source) ].
  ^ dict
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> basicPackageDefinition: packageName [
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName)
    classDefinitions:
      (Dictionary new
        at: 'TestNormalClass'
          put:
            (self definitionOfNormalClassNamed: 'TestNormalClass' inPackage: packageName);
        at: 'TestNormalDbTransientClass'
          put:
            (self
                definitionOfBasicClassNamed: 'TestNormalDbTransientClass'
                type: 'normal'
                instvars: {'instvar1'}
                gs_options: {'dbTransient'}
                inPackage: packageName);
        at: 'TestVariableClass'
          put:
            (self
                definitionOfBasicClassNamed: 'TestVariableClass'
                type: 'variable'
                instvars: {}
                gs_options: nil
                inPackage: packageName);
        at: 'TestMyBagClass'
          put:
            (self
                definitionOfBasicClassNamed: 'TestMyBagClass'
                superclass: 'Bag'
                type: 'normal'
                instvars: {}
                gs_options: {'disallowGciStore'}
                inPackage: packageName);
        at: 'TestCustomByteArrayClass'
          put:
            (self
                definitionOfBasicClassNamed: 'TestCustomByteArrayClass'
                superclass: 'Object'
                type: 'bytes'
                instvars: {}
                gs_options: {}
                inPackage: packageName);
        at: 'TestMyByteArrayClass'
          put:
            (self
                definitionOfBasicClassNamed: 'TestMyByteArrayClass'
                superclass: 'ByteArray'
                type: 'bytes'
                instvars: {}
                gs_options: {}
                inPackage: packageName);
        at: 'TestProtoObjectClass'
          put:
            (self
                definitionOfBasicClassNamed: 'TestProtoObjectClass'
                superclass: 'nil'
                type: 'normal'
                instvars: {}
                gs_options: {}
                inPackage: packageName);
        yourself)
    classExtensions: Dictionary new
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> definitionOfBasicClassNamed: className superclass: superclassName type: classType instvars: instvars gs_options: gs_options inPackage: packageName [
  ^ RwClassDefinition
    withProperties:
      (self
        propertiesForClassNamed: className
        super: superclassName
        instvars: instvars
        classinstvars: {}
        classvars: {}
        type: classType
        gs_options: gs_options
        inPackage: packageName)
    classMethodDefinitions: (self basicClassMethodsForClass: className)
    instanceMethodDefinitions: (self basicInstanceMethodsForClass: className)
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> definitionOfBasicClassNamed: className type: classType instvars: instvars gs_options: gs_options inPackage: packageName [
  ^ self
    definitionOfBasicClassNamed: className
    superclass: 'Object'
    type: classType
    instvars: instvars
    gs_options: gs_options
    inPackage: packageName
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> definitionOfNormalClassNamed: className inPackage: packageName [
  ^ self
    definitionOfBasicClassNamed: className
    type: 'normal'
    instvars: {}
    gs_options: {}
    inPackage: packageName
]

{ #category : 'support - simple definitions' }
RwLoadingTest >> definitionOfSimpleClassNamed: name inPackage: packageName [

	^RwClassDefinition
		withProperties: (self propertiesForSimpleClassNamed: name inPackage: packageName)
		classMethodDefinitions: (self sillyClassMethodsForClassNamed: name)
		instanceMethodDefinitions: (self accessorMethodsForSimpleClassNamed: name)
]

{ #category : 'support' }
RwLoadingTest >> definitionsOfLoadedPackages: packageNames [

	^(Rowan loadedPackagesNamed: packageNames) asDefinition
]

{ #category : 'private' }
RwLoadingTest >> loadAndTestPackagesNamed: packageNames using: myPackageSet [

	|  imagePackageSet1 diff1 imagePackageSet2 diff2 |
	Rowan updateLoadedPackages.
	packageNames do: [:name | self assertAbsenceOfPackage: name].
	imagePackageSet1 := self definitionsOfLoadedPackages: packageNames.
	diff1 := myPackageSet compareAgainstBase: imagePackageSet1.
	self deny: diff1 isEmpty.
	self applyDiffToImage: diff1.
	imagePackageSet2 := self definitionsOfLoadedPackages: packageNames.
	diff2 := myPackageSet compareAgainstBase: imagePackageSet2.
	self assert: diff2 isEmpty
]

{ #category : 'support - simple definitions' }
RwLoadingTest >> package1Definition [

	^RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> 'TestPackage1')
		classDefinitions: (Dictionary
				with: 'TestClass1' -> (self definitionOfSimpleClassNamed: 'TestClass1' inPackage: 'TestPackage1'))
		classExtensions: Dictionary new
]

{ #category : 'support - simple definitions' }
RwLoadingTest >> package2Definition [

	^RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> 'TestPackage2')
		classDefinitions: (Dictionary
				with: 'TestClass2' -> (self definitionOfSimpleClassNamed: 'TestClass2' inPackage: 'TestPackage2'))
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwLoadingTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars type: type gs_options: gs_options inPackage: packageName [
  | dict |
  dict := Dictionary new.
  dict
    at: 'category' put: packageName;
    at: 'classinstvars' put: classinstvars;
    at: 'classvars' put: classvars;
    at: 'comment'
      put:
        'I am a class generated by Cypress tests. My name is ' , className
            , ', what''s yours?';
    at: 'gs_SymbolDictionary' put: 'CypressTestData';
    at: 'instvars' put: instvars;
    at: 'name' put: className;
    at: 'pools' put: Array new;
    at: 'super' put: superClassName;
    at: 'type' put: type.
  (gs_options _isArray and: [gs_options isEmpty not ])
    ifTrue: [ dict at: 'gs_options' put: (gs_options collect: [:each | each asString ]) asSortedCollection asArray ].
  ^ dict
]

{ #category : 'support - simple definitions' }
RwLoadingTest >> propertiesForSimpleClassNamed: className inPackage: packageName [

	| dict |
	dict := Dictionary new.
	dict
		at: 'category' put: packageName;
		at: 'classinstvars' put: Array new;
		at: 'classvars' put: Array new;
		at: 'comment'
			put: 'I am a class generated by Cypress tests. My name is ' , className
					, ', what''s yours?';
		at: 'gs_SymbolDictionary' put: 'CypressTestData';
		at: 'instvars'
			put: (Array with: 'iv1In' , className with: 'iv2In' , className);
		at: 'name' put: className;
		at: 'pools' put: Array new;
		at: 'super' put: 'Object';
		at: 'type' put: 'normal'.
	^dict
]

{ #category : 'running' }
RwLoadingTest >> setUp [

	"Remove the CypressTestData symbol dictionary, if present"

	| user symList dict index |
	user := System myUserProfile.
	symList := user symbolList.
	dict := symList objectNamed: #CypressTestData.
	dict
		ifNotNil: 
			[index := user symbolList indexOf: dict.
			index > 0 ifTrue: [user removeDictionaryAt: index]]
]

{ #category : 'support - simple definitions' }
RwLoadingTest >> sillyClassMethodsForClassNamed: name [

	^Dictionary
		with: #'sillyMessage' -> (RwMethodDefinition
						withProperties: (Dictionary with: 'selector' -> #'sillyMessage'
								with: 'protocol' -> 'silliness')
						source: 'sillyMessage
	^ ''You have sent a silly message to the class '' , name')
]

{ #category : 'tests' }
RwLoadingTest >> testBasicLoading [
  "test loading a variety of different class types"

  | packageName packageNames myPackageSet |
  packageName := 'BasicTestPackage'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage: (self basicPackageDefinition: packageName);
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet
]

{ #category : 'tests' }
RwLoadingTest >> testSimpleLoading [
	"validate functionality loading a simple class"

	| packageNames myPackageSet |
	packageNames := #('TestPackage1' 'TestPackage2').
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage: self package1Definition;
		addPackage: self package2Definition.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.

]
