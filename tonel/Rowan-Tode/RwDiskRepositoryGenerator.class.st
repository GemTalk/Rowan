Class {
	#name : 'RwDiskRepositoryGenerator',
	#superclass : 'Object',
	#category : 'Rowan-Tode'
}

{ #category : 'generating' }
RwDiskRepositoryGenerator class >> generate: aRwLoadSpecification [
  ^ self new
    generate: aRwLoadSpecification;
    yourself
]

{ #category : 'generating' }
RwDiskRepositoryGenerator class >> generateForUrl: specNameOrUrl [
  ^ self new
    generateForUrl: specNameOrUrl;
    yourself
]

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> classMethodsForClass: className [
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod'.
      'class method to be deleted'.
      'testClassMethod ^ true'}.
      {#'testClassMethodSurvivor'.
      'class method to be kept'.
      'testClassMethodSurvivor ^ true'}}
]

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> definitionOfClassNamed: className inPackage: packageName instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions [
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

{ #category : 'generator' }
RwDiskRepositoryGenerator >> generate: myLoadSpecification [
  | myPackageSet |
  myLoadSpecification repositoryUrl
    ifNil: [ 
      | url |
      url := myLoadSpecification projectUrl asRwUrl.
      url scheme = 'file'
        ifFalse: [ 
          self
            error:
              'if ' , myLoadSpecification specName printString
                ,
                  ' has not been attached to a cloned repository (repositoryUrl nonNil), then the projectUrl must be a file: url: '
                , url printString ] ].
  myPackageSet := self generatePackageSet.
  RwTool save savePackageSet: myPackageSet specification: myLoadSpecification
]

{ #category : 'generator' }
RwDiskRepositoryGenerator >> generateForUrl: specNameOrUrl [
  | url |
  url := specNameOrUrl asRwUrl.
  url scheme = 'file'
    ifFalse: [ 
      self
        error:
          'Must use a file: url for spec, since generator will write to the disk repository' ].
  ^ self generate: (RwLoadSpecification fromUrl: url)
]

{ #category : 'generator' }
RwDiskRepositoryGenerator >> generatePackageSet [
  | packageName packageNames myPackageSet className1 className2 classDefinitionsDictionary |
  packageName := 'CombinedTestPackage'.
  className1 := 'TestClass1'.
  className2 := 'TestClass2'.
  packageNames := {packageName}.
  classDefinitionsDictionary := Dictionary new
    at: className1
      put:
        ((self
            definitionOfClassNamed: className1
            inPackage: packageName
            instanceMethodDefinitions:
              (self instanceMethodsForClass: className1)
            classMethodDefinitions: (self classMethodsForClass: className1))
            properties:
                (self
                    propertiesOfClassInitializationClassNamed: className1
                    inPackage: packageName);
            yourself);
    at: className2
      put:
        ((self
            definitionOfClassNamed: className2
            inPackage: packageName
            instanceMethodDefinitions:
              (self instanceMethodsForClass: className2)
            classMethodDefinitions: (self classMethodsForClass: className2))
            properties:
                (self
                    propertiesOfClassInitializationClassNamed: className2
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
  ^ myPackageSet
]

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> instanceMethodsForClass: className [
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod'.
      'instance method to be deleted'.
      'testInstanceMethod ^ true'}.
      {#'testInstanceMethodSurvivor'.
      'instance method to be kept'.
      'testInstanceMethodSurvivor ^ true'}}
]

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> methodDefsFromSpec: specArray [
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

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment gs_SymbolDictionary: ignored pools: pools type: type gs_options: gs_options inPackage: packageName [
  | dict |
  dict := Dictionary new.
  dict
    at: 'category' put: packageName;
    at: 'classinstvars' put: classinstvars;
    at: 'classvars' put: classvars;
    at: 'comment' put: comment;
    at: 'instvars' put: instvars;
    at: 'name' put: className;
    at: 'pools' put: pools;
    at: 'superclass' put: superClassName;
    at: 'type' put: type.
  (gs_options _isArray and: [ gs_options isEmpty not ])
    ifTrue: [ 
      dict
        at: 'gs_options'
        put:
          (gs_options collect: [ :each | each asString ]) asSortedCollection asArray ].
  ^ dict
]

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars type: type gs_options: gs_options inPackage: packageName [
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

{ #category : 'definitions' }
RwDiskRepositoryGenerator >> propertiesOfClassInitializationClassNamed: className inPackage: packageName [
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
