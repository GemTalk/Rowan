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
RwLoadingTest >> basicClassDefinitions: packageName [
  ^ Dictionary new
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
    yourself
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> basicClassMethodsForClass: className [
  ^ self
    methodDefsFromSpec:
      {{#'classMeth'.
      'accessing'.
      ('classMeth ^''classMeth for class ''' , className printString)}}
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> basicInstanceMethodsForClass: className [
  ^ self
    methodDefsFromSpec:
      {{#'instanceMeth'.
      'accessing'.
      ('instanceMeth ^''instanceMeth for class ''' , className printString)}}
]

{ #category : 'support - basic definitions' }
RwLoadingTest >> basicPackageDefinition: packageName [
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName)
    classDefinitions: (self basicClassDefinitions: packageName)
    classExtensions: Dictionary new
]

{ #category : 'support - classInitialization definitions' }
RwLoadingTest >> classInitializationClassMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {
			{#'initialize'.
      		'initialization'.
      		('initialize ClassVar1 := classInstVar1 := 1' )}.
			{#'classVar'.
      		'accessing'.
      		('classVar ^ ClassVar1' )}.
			{#'classInstVar1'.
      		'accessing'.
      		('classInstVar1 ^ classInstVar1' )}.
			{#'classInstVar2'.
      		'accessing'.
      		('classInstVar2 ^ classInstVar2' )}.
		}
]

{ #category : 'support - classInitialization definitions' }
RwLoadingTest >> classInitializationClassMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {
			{#'initialize'.
      		'initialization'.
      		('initialize ClassVar1 := classInstVar1 := 1. classInstVar2 := 2' )}.
			{#'classVar'.
      		'accessing'.
      		('classVar ^ ClassVar1' )}.
			{#'classInstVar1'.
      		'accessing'.
      		('classInstVar1 ^ classInstVar1' )}.
			{#'classInstVar2'.
      		'accessing'.
      		('classInstVar2 ^ classInstVar2' )}.
		}
]

{ #category : 'support - classInitialization definitions' }
RwLoadingTest >> classInitializePackageDefinition: packageName className: className classMethodDefinitions: classMethodDefinitions [
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName)
    classDefinitions:
      (Dictionary new
        at: className
          put:
            (self
                definitionOfClassInitializationClassNamed: className
                inPackage: packageName
                classMethodDefinitions: classMethodDefinitions);
        yourself)
    classExtensions: Dictionary new
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions1: packageName [
  ^ self classPropertyChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions1: packageName in: classDefinitionDictionary [
  ^ classDefinitionDictionary
    at: 'TestAddInstVarClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestAddInstVarClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestAddClassInstVarClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestAddClassInstVarClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestAddClassVarClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestAddClassVarClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestChangeSuperclassClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestChangeSuperclassClass'
                super: 'Object'
                instvars: {'instVar1'}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestChangeCommentClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestChangeCommentClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestChangePoolsClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestChangePoolsClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    yourself
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions2: packageName [
  ^ self classPropertyChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions2: packageName in: classDefinitionDictionary [
  ^ classDefinitionDictionary
    at: 'TestAddInstVarClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestAddInstVarClass'
                super: 'Object'
                instvars: {'instVar1'}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestAddClassInstVarClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestAddClassInstVarClass'
                super: 'Object'
                instvars: {}
                classinstvars: {'classInstVar1'}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestAddClassVarClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestAddClassVarClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {'ClassVar1'}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestChangeSuperclassClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestChangeSuperclassClass'
                super: 'Association'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestChangeCommentClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestChangeCommentClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a new comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    at: 'TestChangePoolsClass'
      put:
        (RwClassDefinition
            withProperties:
              (self
                propertiesForClassNamed: 'TestChangePoolsClass'
                super: 'Object'
                instvars: {}
                classinstvars: {}
                classvars: {}
                comment: 'a comment'
                pools: {}
                inPackage: packageName)
            classMethodDefinitions: Dictionary new
            instanceMethodDefinitions: Dictionary new);
    yourself
]

{ #category : 'support - combined definitions' }
RwLoadingTest >> combinedClassMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {{#'initialize'.
      'initialization'.
      'initialize ClassVar1 := classInstVar1 := true'}.
      {#'testClassMethod1'.
      'xxx'.
      'testClassMethod1 ^ ClassVar1'}.
      {#'testClassMethod2'.
      'xxx'.
      'testClassMethod2 ^ classInstVar1'}.
      {#'testClassMethod3'.
      'xxx'.
      'testClassMethod3 ^ 1'}}
]

{ #category : 'support - combined definitions' }
RwLoadingTest >> combinedClassMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {{#'initialize'.
      'initialization'.
      'initialize ClassVar1 := classInstVar1 := true'}.
      {#'testClassMethod1'.
      'xxx'.
      'testClassMethod1 ^ ClassVar1'}.
      {#'testClassMethod3'.
      'yyy'.
      'testClassMethod3 ^ 2'}}
]

{ #category : 'support - combined definitions' }
RwLoadingTest >> combinedInstanceMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'xxx'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod2'.
      'xxx'.
      'testInstanceMethod2 ^ true'}.
      {#'testInstanceMethod3'.
      'xxx'.
      'testInstanceMethod3 ^ 1'}}
]

{ #category : 'support - combined definitions' }
RwLoadingTest >> combinedInstanceMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'xxx'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod3'.
      'yyy'.
      'testInstanceMethod3 ^ 2'}}
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

{ #category : 'support - classInitialization definitions' }
RwLoadingTest >> definitionOfClassInitializationClassNamed: className inPackage: packageName classMethodDefinitions: classMethodDefinitions [
  ^ RwClassDefinition
    withProperties:
      (self
        propertiesOfClassInitializationClassNamed: className
        inPackage: packageName)
    classMethodDefinitions: classMethodDefinitions
    instanceMethodDefinitions: Dictionary new
]

{ #category : 'support - methodDeletion definitions' }
RwLoadingTest >> definitionOfMethodDeletionClassNamed: className inPackage: packageName instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions [
  ^ RwClassDefinition
    withProperties:
      (self
        propertiesForClassNamed: className
        super: 'Object'
        instvars: {}
        classinstvars: {}
        classvars: {}
        type: 'normal'
        gs_options: {}
        inPackage: packageName)
    classMethodDefinitions: classMethodDefinitions
    instanceMethodDefinitions: instanceMethodDefinitions
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

{ #category : 'support' }
RwLoadingTest >> loadAndTestPackagesNamed: packageNames before: beforePackageSet after: afterPackageSet [

	|  imagePackageSet1 diff1 imagePackageSet2 diff2 |
	Rowan updateLoadedPackages.
	imagePackageSet1 := self definitionsOfLoadedPackages: packageNames.
	diff1 := beforePackageSet compareAgainstBase: imagePackageSet1.
	self deny: diff1 isEmpty.
	self applyDiffToImage: diff1.
	imagePackageSet2 := self definitionsOfLoadedPackages: packageNames.
	diff2 := afterPackageSet compareAgainstBase: imagePackageSet2.
	self assert: diff2 isEmpty
]

{ #category : 'support' }
RwLoadingTest >> loadAndTestPackagesNamed: packageNames loaded: packagesShouldBeLoaded using: myPackageSet [ 

	|  imagePackageSet1 diff1 imagePackageSet2 diff2 |
	Rowan updateLoadedPackages.
	packagesShouldBeLoaded
		ifFalse: [  packageNames do: [:name | self assertAbsenceOfPackage: name ] ].
	imagePackageSet1 := self definitionsOfLoadedPackages: packageNames.
	diff1 := myPackageSet compareAgainstBase: imagePackageSet1.
	self deny: diff1 isEmpty.
	self applyDiffToImage: diff1.
	imagePackageSet2 := self definitionsOfLoadedPackages: packageNames.
	diff2 := myPackageSet compareAgainstBase: imagePackageSet2.
	self assert: diff2 isEmpty
]

{ #category : 'support' }
RwLoadingTest >> loadAndTestPackagesNamed: packageNames using: myPackageSet [

	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet
]

{ #category : 'support' }
RwLoadingTest >> methodDefsFromSpec: specArray [
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

{ #category : 'support - methodDeletion definitions' }
RwLoadingTest >> methodDeletionClassMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod'.
      'class method to be deleted'.
      'testClassMethod ^ true'}.
      {#'testClassMethodSurvivor'.
      'class method to be kept'.
      'testClassMethodSurvivor ^ true'}}
]

{ #category : 'support - methodDeletion definitions' }
RwLoadingTest >> methodDeletionClassMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethodSurvivor'.
      'class method to be kept'.
      'testClassMethodSurvivor ^ true'}}
]

{ #category : 'support - methodDeletion definitions' }
RwLoadingTest >> methodDeletionInstanceMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod'.
      'instance method to be deleted'.
      'testInstanceMethod ^ true'}.
      {#'testInstanceMethodSurvivor'.
      'instance method to be kept'.
      'testInstanceMethodSurvivor ^ true'}}
]

{ #category : 'support - methodDeletion definitions' }
RwLoadingTest >> methodDeletionInstanceMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethodSurvivor'.
      'instance method to be kept'.
      'testInstanceMethodSurvivor ^ true'}}
]

{ #category : 'support - methodDeletion definitions' }
RwLoadingTest >> methodDeletionPackageDefinition: packageName className: className instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions [
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName)
    classDefinitions:
      (Dictionary new
        at: className
          put:
            (self
                definitionOfMethodDeletionClassNamed: className
                inPackage: packageName
                instanceMethodDefinitions: instanceMethodDefinitions
                classMethodDefinitions: classMethodDefinitions);
        yourself)
    classExtensions: Dictionary new
]

{ #category : 'support - methodProtocolChange definitions' }
RwLoadingTest >> methodProtocolChangeClassMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod1'.
      'protocol A'.
      'testClassMethod1 ^ true'}.
      {#'testClassMethod2'.
      'protocol A'.
      'testClassMethod2 ^ true'}}
]

{ #category : 'support - methodProtocolChange definitions' }
RwLoadingTest >> methodProtocolChangeClassMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod1'.
      'protocol A'.
      'testClassMethod1 ^ true'}.
      {#'testClassMethod2'.
      'protocol B'.
      'testClassMethod2 ^ true'}}
]

{ #category : 'support - methodProtocolChange definitions' }
RwLoadingTest >> methodProtocolChangeInstanceMethodsForClass1: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'protocol A'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod2'.
      'protocol A'.
      'testInstanceMethod2 ^ true'}}
]

{ #category : 'support - methodProtocolChange definitions' }
RwLoadingTest >> methodProtocolChangeInstanceMethodsForClass2: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'protocol A'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod2'.
      'protocol B'.
      'testInstanceMethod2 ^ true'}}
]

{ #category : 'support - methodProtocolChange definitions' }
RwLoadingTest >> methodProtocolChangePackageDefinition: packageName className: className instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions [
  ^ self
    methodDeletionPackageDefinition: packageName
    className: className
    instanceMethodDefinitions: instanceMethodDefinitions
    classMethodDefinitions: classMethodDefinitions
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
RwLoadingTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName [
  | dict |
  dict := Dictionary new.
  dict
    at: 'category' put: packageName;
    at: 'classinstvars' put: classinstvars;
    at: 'classvars' put: classvars;
    at: 'comment' put: comment;
    at: 'gs_SymbolDictionary' put: gs_SymbolDictionary;
    at: 'instvars' put: instvars;
    at: 'name' put: className;
    at: 'pools' put: pools;
    at: 'super' put: superClassName;
    at: 'type' put: type.
  (gs_options _isArray and: [ gs_options isEmpty not ])
    ifTrue: [ 
      dict
        at: 'gs_options'
        put:
          (gs_options collect: [ :each | each asString ]) asSortedCollection asArray ].
  ^ dict
]

{ #category : 'support' }
RwLoadingTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment pools: pools inPackage: packageName [
  ^ self
    propertiesForClassNamed: className
    super: superClassName
    instvars: instvars
    classinstvars: classinstvars
    classvars: classvars
    comment: comment
    gs_SymbolDictionary: 'CypressTestData'
    pools: pools
    type: 'normal'
    gs_options: {}
    inPackage: packageName
]

{ #category : 'support' }
RwLoadingTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars type: type gs_options: gs_options inPackage: packageName [
  ^ self
    propertiesForClassNamed: className
    super: superClassName
    instvars: instvars
    classinstvars: classinstvars
    classvars: classvars
    comment:
      'I am a class generated by Cypress tests. My name is ' , className
        , ', what''s yours?'
    gs_SymbolDictionary: 'CypressTestData'
    pools: {}
    type: type
    gs_options: gs_options
    inPackage: packageName
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

{ #category : 'support - classInitialization definitions' }
RwLoadingTest >> propertiesOfClassInitializationClassNamed: className inPackage: packageName [
  ^ self
    propertiesForClassNamed: className
    super: 'Object'
    instvars: {}
    classinstvars:
      {'classInstVar1'.
      'classInstVar2'}
    classvars: {'ClassVar1'}
    type: 'normal'
    gs_options: {}
    inPackage: packageName
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
RwLoadingTest >> testClassDelete [
  "test class deletion"

  | packageName packageNames myPackageSet className testClass testInstance |
  packageName := 'ClassDeletionTestPackage'.
  className := 'TestClassDeletionClass'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            methodDeletionPackageDefinition: packageName
            className: className
            instanceMethodDefinitions:
              (self methodDeletionInstanceMethodsForClass1: className)
            classMethodDefinitions:
              (self methodDeletionClassMethodsForClass1: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  testClass := System myUserProfile symbolList objectNamed: className.
  testInstance := testClass new.
  self
    assert:
        (testClass class compiledMethodAt: #'testClassMethod' otherwise: false) ~~ false;
    assert: testClass testClassMethod;
    assert: testClass testClassMethodSurvivor.
  self
    assert:
        (testClass compiledMethodAt: #'testInstanceMethod' otherwise: false) ~~ false;
    assert: testInstance testInstanceMethod;
    assert: testInstance testInstanceMethodSurvivor.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
	myPackageSet
    addPackage:
        (RwPackageDefinition
            withProperties: (Dictionary with: 'name' -> packageName)
            classDefinitions: Dictionary new
            classExtensions: Dictionary new);
    yourself.
  self 
		loadAndTestPackagesNamed: packageNames 
		before: myPackageSet 
		after: RwPackageSetDefinition new. "the package will be deleted after load when updatePackagesFromCategories is called, because last class with that category no longer exists -- implicit delete package"
  self assert: (System myUserProfile symbolList objectNamed: className) isNil
]

{ #category : 'tests' }
RwLoadingTest >> testClassInitialization1 [
  "test that class #initialize method is run when the class #initialize method is added"

  | packageName packageNames myPackageSet className testClass |
  packageName := 'ClassInitializationTestPackage'.
  className := 'TestClassInitializationClass'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            classInitializePackageDefinition: packageName
            className: className
            classMethodDefinitions:
              (self classInitializationClassMethodsForClass1: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  testClass := System myUserProfile symbolList objectNamed: className.
  self assert: testClass classVar = 1.
  self assert: testClass classInstVar1 = 1
]

{ #category : 'tests' }
RwLoadingTest >> testClassInitialization2 [
  "test that class #initialize method is run when the class #initialize method is changed"

  | packageName packageNames myPackageSet className testClass |
  packageName := 'ClassInitializationTestPackage'.
  className := 'TestClassInitializationClass'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            classInitializePackageDefinition: packageName
            className: className
            classMethodDefinitions:
              (self classInitializationClassMethodsForClass1: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  testClass := System myUserProfile symbolList objectNamed: className.
  self assert: testClass classVar = 1.
  self assert: testClass classInstVar1 = 1.
  self assert: testClass classInstVar2 isNil.	"update the initialization method ... #initialize should run again"
  packageName := 'ClassInitializationTestPackage'.
  className := 'TestClassInitializationClass'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            classInitializePackageDefinition: packageName
            className: className
            classMethodDefinitions:
              (self classInitializationClassMethodsForClass2: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
  self assert: testClass classVar = 1.
  self assert: testClass classInstVar1 = 1.
  self assert: testClass classInstVar2 = 2
]

{ #category : 'tests' }
RwLoadingTest >> testClassPropertyChanges [
  "test class property changes -- skipping pools and no methods involved ... for now"

  | packageName packageNames myPackageSet className |
  packageName := 'ClassPropertyChangesTestPackage'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (RwPackageDefinition
            withProperties: (Dictionary with: 'name' -> packageName)
            classDefinitions:
              (self classPropertyChangeClassDefinitions1: packageName)
            classExtensions: Dictionary new);
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (RwPackageDefinition
            withProperties: (Dictionary with: 'name' -> packageName)
            classDefinitions:
              (self classPropertyChangeClassDefinitions2: packageName)
            classExtensions: Dictionary new);
    yourself.
  self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet
]

{ #category : 'tests' }
RwLoadingTest >> testCombinedChanges [
  "single package update that includes an example of each kind of change supported"

  | packageName packageNames myPackageSet className testClass testInstance classDefinitionsDictionary deletedClassName |
  packageName := 'CombinedTestPackage'.
  className := 'TestCombinedChangesClass'.
  deletedClassName := 'TestClassDeletionClass'.
  packageNames := {packageName}.
  classDefinitionsDictionary := (self basicClassDefinitions: packageName)
    at: className
      put:
        ((self
            definitionOfMethodDeletionClassNamed: className
            inPackage: packageName
            instanceMethodDefinitions:
              (self combinedInstanceMethodsForClass1: className)
            classMethodDefinitions:
              (self combinedClassMethodsForClass1: className))
            properties:
                (self
                    propertiesOfClassInitializationClassNamed: className
                    inPackage: packageName);
            yourself);
    at: deletedClassName
      put:
        ((self
            definitionOfMethodDeletionClassNamed: deletedClassName
            inPackage: packageName
            instanceMethodDefinitions:
              (self combinedInstanceMethodsForClass1: deletedClassName)
            classMethodDefinitions:
              (self combinedClassMethodsForClass1: deletedClassName))
            properties:
                (self
                    propertiesOfClassInitializationClassNamed: deletedClassName
                    inPackage: packageName);
            yourself);
    yourself.
  myPackageSet := RwPackageSetDefinition new
    addPackage:
        (RwPackageDefinition
            withProperties: (Dictionary with: 'name' -> packageName)
            classDefinitions: classDefinitionsDictionary
            classExtensions: Dictionary new);
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  testClass := System myUserProfile symbolList objectNamed: className.
  testInstance := testClass new.
  self
    assert: testClass testClassMethod1;
    assert: testClass testClassMethod2;
    assert: testClass testClassMethod3 = 1;
    assert: testInstance testInstanceMethod1;
    assert: testInstance testInstanceMethod2;
    assert: testInstance testInstanceMethod3 = 1.
  packageNames := {packageName}.
  classDefinitionsDictionary := (self basicClassDefinitions: packageName)
    at: className
      put:
        ((self
            definitionOfMethodDeletionClassNamed: className
            inPackage: packageName
            instanceMethodDefinitions:
              (self combinedInstanceMethodsForClass2: className)
            classMethodDefinitions:
              (self combinedClassMethodsForClass2: className))
            properties:
                (self
                    propertiesOfClassInitializationClassNamed: className
                    inPackage: packageName);
            yourself);
    yourself.
  myPackageSet := RwPackageSetDefinition new
    addPackage:
        (RwPackageDefinition
            withProperties: (Dictionary with: 'name' -> packageName)
            classDefinitions: classDefinitionsDictionary
            classExtensions: Dictionary new);
    yourself.
  self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
  self
    assert: testClass testClassMethod1;
    assert:
        (testClass class compiledMethodAt: #'testClassMethod2' otherwise: false)
            == false;
    assert: testClass testClassMethod3 = 2;
    assert: testInstance testInstanceMethod1;
    assert:
        (testClass compiledMethodAt: #'testInstanceMethod2' otherwise: false) == false;
    assert: testInstance testInstanceMethod3 = 2
]

{ #category : 'tests' }
RwLoadingTest >> testMethodDelete [
  "test class and instance method deletion"

  | packageName packageNames myPackageSet className testClass testInstance |
  packageName := 'MethodDeletionTestPackage'.
  className := 'TestMethodDeletionClass'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            methodDeletionPackageDefinition: packageName
            className: className
            instanceMethodDefinitions:
              (self methodDeletionInstanceMethodsForClass1: className)
            classMethodDefinitions:
              (self methodDeletionClassMethodsForClass1: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  testClass := System myUserProfile symbolList objectNamed: className.
  testInstance := testClass new.
  self
    assert:
        (testClass class compiledMethodAt: #'testClassMethod' otherwise: false) ~~ false;
    assert: testClass testClassMethod;
    assert: testClass testClassMethodSurvivor.
  self
    assert:
        (testClass compiledMethodAt: #'testInstanceMethod' otherwise: false) ~~ false;
    assert: testInstance testInstanceMethod;
    assert: testInstance testInstanceMethodSurvivor.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            methodDeletionPackageDefinition: packageName
            className: className
            instanceMethodDefinitions:
              (self methodDeletionInstanceMethodsForClass2: className)
            classMethodDefinitions:
              (self methodDeletionClassMethodsForClass2: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
  self
    assert:
        (testClass class compiledMethodAt: #'testClassMethod' otherwise: false) == false;
    assert: testClass testClassMethodSurvivor.
  self
    assert:
        (testClass compiledMethodAt: #'testInstanceMethod' otherwise: false) == false;
    assert: testInstance testInstanceMethodSurvivor
]

{ #category : 'tests' }
RwLoadingTest >> testMethodProtocolChange [
  "test class and instance method protocol changes"

  | packageName packageNames myPackageSet className testClass testInstance |
  packageName := 'MethodProtocolChangedTestPackage'.
  className := 'TestMethodProtocolChangeClass'.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            methodProtocolChangePackageDefinition: packageName
            className: className
            instanceMethodDefinitions:
              (self methodProtocolChangeInstanceMethodsForClass1: className)
            classMethodDefinitions:
              (self methodProtocolChangeClassMethodsForClass1: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames using: myPackageSet.
  testClass := System myUserProfile symbolList objectNamed: className.
  testInstance := testClass new.
  self
    assert: testClass testClassMethod1;
    assert: testClass testClassMethod2;
    assert: testInstance testInstanceMethod1;
    assert: testInstance testInstanceMethod2.
  packageNames := {packageName}.
  myPackageSet := RwPackageSetDefinition new.
  myPackageSet
    addPackage:
        (self
            methodProtocolChangePackageDefinition: packageName
            className: className
            instanceMethodDefinitions:
              (self methodProtocolChangeInstanceMethodsForClass2: className)
            classMethodDefinitions:
              (self methodProtocolChangeClassMethodsForClass2: className));
    yourself.
  self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
  self
    assert: testClass testClassMethod1;
    assert: testClass testClassMethod2;
    assert: testInstance testInstanceMethod1;
    assert: testInstance testInstanceMethod2
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
