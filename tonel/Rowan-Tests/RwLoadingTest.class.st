"
No class-specific documentation for CypLoadingTest, hierarchy is: 
Object
  TestAsserter
    TestCase( testSelector)
      CypLoadingTest

"
Class {
	#name : 'RwLoadingTest',
	#superclass : 'RwAbstractTest',
	#classVars : [
		'PoolDictionaryDictionary'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'running' }
RwLoadingTest class >> debug [

	^self allTestSelectors do: [:selector | self debug: selector]
]

{ #category : 'initlization' }
RwLoadingTest class >> initialize [
	"self initialize"

	"cannot create useful pool dictionaries on the fly, so need  a set of 
	 canonical pool dictionaries."

	PoolDictionaryDictionary := Dictionary new.
	{(SymbolDictionary new
		at: #'Carrot1' put: 1;
		at: #'Radish1' put: 2;
		name: #'PoolDictionary1';
		yourself).
	(SymbolDictionary new
		at: #'Carrot2' put: 1;
		at: #'Radish2' put: 2;
		name: #'PoolDictionary2';
		yourself).
	(SymbolDictionary new
		at: #'Carrot3' put: 1;
		at: #'Radish3' put: 2;
		name: #'PoolDictionary3';
		yourself)} do: [ :pd | PoolDictionaryDictionary at: pd name put: pd ]
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

	"work out the process handling that is needed in the face of new class versions"

	| visitor patchSet |
	visitor := RwGsImagePatchVisitor newFor: self _symbolDictionary.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	patchSet classesWithNewVersions
		do: [ :each | each newClassVersionPatches: aPackageSetModification patchSet: nil ].
	visitor := RwGsImagePatchVisitor newFor: self _symbolDictionary.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet apply
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

{ #category : 'support - classExtensions definitions' }
RwLoadingTest >> classExtentionsInstanceMethodsForClass2: className inPackage: packageName [

	^ self
		methodDefsFromSpec:
			{{#'foo'.
			'*', packageName.
			'foo ^ ''foo'''}}
]

{ #category : 'support - classExtensions definitions' }
RwLoadingTest >> classExtionsClassDefinitions1: packageName className: className in: classDefinitionDictionary [

	"Class extension methods"

	^ classDefinitionDictionary
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
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

{ #category : 'support - classExtensions definitions' }
RwLoadingTest >> classExtionsClassDefinitions2: packageName className: className in: classDefinitionDictionary [

	"Class extension methods"

	^ classDefinitionDictionary
		at: className
			put:
				(RwClassExtensionDefinition
						withProperties: (self propertiesForClassExtensionNamed: className)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self classExtentionsInstanceMethodsForClass2: className inPackage: packageName));
		yourself
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

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangeClassDefinitions1: packageName in: classDefinitionDictionary [
	"Class Inst Var Changes: "

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
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

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangeClassDefinitions2: packageName in: classDefinitionDictionary [
	"Class Inst Var Changes: add classInstVar1"

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {'classInstVar1'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangeClassDefinitions3: packageName in: classDefinitionDictionary [
	"Class Inst Var Changes: add classInstVar2"

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars:
									{'classInstVar1'.
									'classInstVar2'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangeClassDefinitions4: packageName in: classDefinitionDictionary [
	"Class Inst Var Changes: add classInstVar3 and remove classInstVar1"

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars:
									{'classInstVar2'.
									'classInstVar3'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangesClassDefinitions1: packageName [
	^ self classInstVarChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangesClassDefinitions2: packageName [
	^ self classInstVarChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangesClassDefinitions3: packageName [
	^ self classInstVarChangeClassDefinitions3: packageName in: Dictionary new
]

{ #category : 'support - classInstVarChanges definitions' }
RwLoadingTest >> classInstVarChangesClassDefinitions4: packageName [
	^ self classInstVarChangeClassDefinitions4: packageName in: Dictionary new
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions1: packageName [
  ^ self classPropertyChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions1: packageName in: classDefinitionDictionary [
	"Class Property Changes that do not result in new class versions "

	^ classDefinitionDictionary
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
		yourself
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions2: packageName [
  ^ self classPropertyChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - classProperyChange definitions' }
RwLoadingTest >> classPropertyChangeClassDefinitions2: packageName in: classDefinitionDictionary [
	"Class Property Changes that do not result in new class versions "

	^ classDefinitionDictionary
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
		yourself
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangeClassDefinitions1: packageName in: classDefinitionDictionary [
	"Class Var Changes: "

	^ classDefinitionDictionary
		at: 'TestClassVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassVarClass'
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

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangeClassDefinitions2: packageName in: classDefinitionDictionary [
	"Class Var Changes: add ClassVar1"

	^ classDefinitionDictionary
		at: 'TestClassVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {'ClassVar1'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangeClassDefinitions3: packageName in: classDefinitionDictionary [
	"Class Var Changes: add ClassVar2"

	^ classDefinitionDictionary
		at: 'TestClassVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars:
									{'ClassVar1'.
									'ClassVar2'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangeClassDefinitions4: packageName in: classDefinitionDictionary [
	"Class Var Changes: add ClassVar3 and remove ClassVar1"

	^ classDefinitionDictionary
		at: 'TestClassVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars:
									{'ClassVar2'.
									'ClassVar3'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangesClassDefinitions1: packageName [
	^ self classVarChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangesClassDefinitions2: packageName [
	^ self classVarChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangesClassDefinitions3: packageName [
	^ self classVarChangeClassDefinitions3: packageName in: Dictionary new
]

{ #category : 'support - classVarChanges definitions' }
RwLoadingTest >> classVarChangesClassDefinitions4: packageName [
	^ self classVarChangeClassDefinitions4: packageName in: Dictionary new
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeAssociationInstanceMethodsForClass3: className [
	^ self
		methodDefsFromSpec:
			{{#'customValue'.
			'accessing'.
			'customValue ^value'}}
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassDefinitions1: packageName [
	^ self classVersionChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassDefinitions1: packageName in: classDefinitionDictionary [
	"Class Property Changes that result in new class versions "

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

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassDefinitions2: packageName [
	^ self classVersionChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassDefinitions2: packageName in: classDefinitionDictionary [
	"Class Property Changes that result in new class versions "

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

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassDefinitions3: packageName [
	^ self classVersionChangeClassDefinitions3: packageName in: Dictionary new
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassDefinitions3: packageName in: classDefinitionDictionary [
	"Class Property Changes that result in new class versions "

	"add methods that reference the new class properties"

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
						instanceMethodDefinitions:
							(self classVersionChangeInstVarMethodsForClass3: 'TestAddInstVarClass'));
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
						classMethodDefinitions:
							(self classVersionChangeClassInstVarMethodsForClass3: 'TestAddInstVarClass')
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
						classMethodDefinitions:
							(self classVersionChangeClassVarMethodsForClass3: 'TestAddClassVarClass')
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
						instanceMethodDefinitions:
							(self
								classVersionChangeAssociationInstanceMethodsForClass3:
									'TestChangeSuperclassClass'));
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

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassInstVarMethodsForClass3: className [
	^ self
		methodDefsFromSpec:
			{{#'classInstVar1'.
			'accessing'.
			'classInstVar1 ^classInstVar1'}}
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeClassVarMethodsForClass3: className [
	^ self
		methodDefsFromSpec:
			{{#'classVar1'.
			'accessing'.
			'classVar1 ^ClassVar1'}}
]

{ #category : 'support - classVersionChanges' }
RwLoadingTest >> classVersionChangeInstVarMethodsForClass3: className [
	^ self
		methodDefsFromSpec:
			{{#'instVar1'.
			'accessing'.
			'instVar1 ^instVar1'}}
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

{ #category : 'running' }
RwLoadingTest >> expectedFailures [

	^ {#'testPoolDictionaryChanges'}
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangeClassDefinitions1: packageName in: classDefinitionDictionary [
	"Inst Var Changes: "

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
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

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangeClassDefinitions2: packageName in: classDefinitionDictionary [
	"Inst Var Changes: add instVar1"

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars: {'instVar1'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangeClassDefinitions3: packageName in: classDefinitionDictionary [
	"Inst Var Changes: add instVar2"

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars:
									{'instVar1'.
									'instVar2'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangeClassDefinitions4: packageName in: classDefinitionDictionary [
	"Inst Var Changes: add instVar3 and remove instVar1"

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars:
									{'instVar2'.
									'instVar3'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangesClassDefinitions1: packageName [
	^ self instVarChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangesClassDefinitions2: packageName [
	^ self instVarChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangesClassDefinitions3: packageName [
	^ self instVarChangeClassDefinitions3: packageName in: Dictionary new
]

{ #category : 'support - instVarChanges definitions' }
RwLoadingTest >> instVarChangesClassDefinitions4: packageName [
	^ self instVarChangeClassDefinitions4: packageName in: Dictionary new
]

{ #category : 'support' }
RwLoadingTest >> loadAndTestPackagesNamed: packageNames before: beforePackageSet after: afterPackageSet [

	| imagePackageSet1 diff1 imagePackageSet2 diff2 |
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

	| imagePackageSet1 diff1 imagePackageSet2 diff2 |
	packagesShouldBeLoaded
		ifFalse: [ packageNames do: [ :name | self assertAbsenceOfPackage: name ] ].
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

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangeClassDefinitions1: packageName in: classDefinitionDictionary [
	"Pool Dictionary Changes: "

	^ classDefinitionDictionary
		at: 'TestPoolDictionaryClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestPoolDictionaryClass'
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

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangeClassDefinitions2: packageName in: classDefinitionDictionary [
	"Pool Dictionary Changes: add PD1"

	^ classDefinitionDictionary
		at: 'TestPoolDictionaryClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestPoolDictionaryClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {(PoolDictionaryDictionary at: #'PoolDictionary1')}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangeClassDefinitions3: packageName in: classDefinitionDictionary [
	"Pool Dictionary Changes: add PD2"

	^ classDefinitionDictionary
		at: 'TestPoolDictionaryClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestPoolDictionaryClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools:
									{(PoolDictionaryDictionary at: #'PoolDictionary1').
									(PoolDictionaryDictionary at: #'PoolDictionary2')}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangeClassDefinitions4: packageName in: classDefinitionDictionary [
	"Pool Dictionary Changes: add PD3 and remove PD1"

	^ classDefinitionDictionary
		at: 'TestPoolDictionaryClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestPoolDictionaryClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools:
									{(PoolDictionaryDictionary at: #'PoolDictionary2').
									(PoolDictionaryDictionary at: #'PoolDictionary3')}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangesClassDefinitions1: packageName [
	^ self poolDictionaryChangeClassDefinitions1: packageName in: Dictionary new
]

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangesClassDefinitions2: packageName [
	^ self poolDictionaryChangeClassDefinitions2: packageName in: Dictionary new
]

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangesClassDefinitions3: packageName [
	^ self poolDictionaryChangeClassDefinitions3: packageName in: Dictionary new
]

{ #category : 'support - poolDictionaryChanges definitions' }
RwLoadingTest >> poolDictionaryChangesClassDefinitions4: packageName [
	^ self poolDictionaryChangeClassDefinitions4: packageName in: Dictionary new
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
		gs_SymbolDictionary: self _symbolDictionaryName asString
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
		gs_SymbolDictionary: self _symbolDictionaryName asString
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
			put:
				'I am a class generated by Cypress tests. My name is ' , className
						, ', what''s yours?';
		at: 'gs_SymbolDictionary' put: self _symbolDictionaryName asString;
		at: 'instvars'
			put: (Array with: 'iv1In' , className with: 'iv2In' , className);
		at: 'name' put: className;
		at: 'pools' put: Array new;
		at: 'superclass' put: 'Object';
		at: 'type' put: 'normal'.
	^ dict
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

{ #category : 'support - simple definitions' }
RwLoadingTest >> sillyClassMethodsForClassNamed: name [

	^Dictionary
		with: #'sillyMessage' -> (RwMethodDefinition
						withProperties: (Dictionary with: 'selector' -> #'sillyMessage'
								with: 'protocol' -> 'silliness')
						source: 'sillyMessage
	^ ''You have sent a silly message to the class '' , name')
]

{ #category : 'support - singleClassVersionChange definitions' }
RwLoadingTest >> singleClassVersionChangeClassDefinitions1: packageName className: className [

	"Single class with a method #foo. and new class version"

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #()
								classinstvars: #()
								classvars: #()
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'accessing' 'foo ^''foo''')}));
		yourself
]

{ #category : 'support - singleClassVersionChange definitions' }
RwLoadingTest >> singleClassVersionChangeClassDefinitions2: packageName className: className [

	"Single class with method foo and new class version."

	"add iva and preserve foo unchanged. "

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #('iva')
								classinstvars: #()
								classvars: #()
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'accessing' 'foo ^''foo''')}));
		yourself
]

{ #category : 'support - singleClassVersionChange definitions' }
RwLoadingTest >> singleClassVersionChangeClassDefinitions3: packageName className: className [

	"Single class with method foo and new class version."

	"add civa and change foo. "

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #('iva')
								classinstvars: #('civa')
								classvars: #()
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'accessing' 'foo ^''bar''')}));
		yourself
]

{ #category : 'support - singleClassVersionChange definitions' }
RwLoadingTest >> singleClassVersionChangeClassDefinitions4: packageName className: className [

	"Single class with method foo and new class version."

	"add civa and change protocol of foo. "

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #('iva')
								classinstvars: #('civa')
								classvars: #('CVA')
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'fooing' 'foo ^''foo''')}));
		yourself
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
RwLoadingTest >> testClassAdd [

	"test class addition"

	| packageName packageNames myPackageSet className testClass loadedClass |
	packageName := 'ClassAdditionTestPackage'.
	className := 'TestClassAdditionClass'.
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

	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	loadedClass := self _symbolDictionary
		existingForClass: testClass
		ifAbsent: [ 
			"expected to find a loaded class"
			self assert: false ].
	self assert: testClass == loadedClass handle
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
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
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
		after: RwPackageSetDefinition new.	"the package will be deleted after load when updatePackagesFromCategories is called, because last class with that category no longer exists -- implicit delete package"
	self
		assert:
			(RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol) isNil
]

{ #category : 'tests' }
RwLoadingTest >> testClassExtension [

	"test class extenstion methods"

	| packageName1 packageName2 packageNames myPackageSet className testClass testInstance |
	className := 'TestClassExtensionClass'.
	packageName1 := 'ClassExtensionsBaseTestPackage'.
	packageName2 := 'ClassExtensionsExtensionTestPackage'.
	packageNames := {packageName1}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName1)
						classDefinitions:
							(self
								classExtionsClassDefinitions1: packageName1
								className: className
								in: Dictionary new)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self should: [ testInstance foo ] raise: MessageNotUnderstood.
	packageNames := {packageName2}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName2)
						classDefinitions: Dictionary new
						classExtensions:
							(self
								classExtionsClassDefinitions2: packageName2
								className: className
								in: Dictionary new));
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	self assert: testInstance foo = 'foo'
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
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
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
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
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
RwLoadingTest >> testClassInstVarChanges [

	"characterize class inst var changes with respect to new version creation"

	"as of GemStone 3.4, .... any change to the class inst vars creates a new class version"

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 civs1 civs2 civs3 civs4 |
	packageName := 'ClassInstVarChangesTestPackage'.
	className := 'TestClassInstVarClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classInstVarChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	civs1 := testClass1 _classInstVars copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classInstVarChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	civs2 := testClass2 _classInstVars copy.
	self deny: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classInstVarChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	civs3 := testClass3 _classInstVars copy.
	self deny: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classInstVarChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	civs4 := testClass4 _classInstVars copy.
	self deny: testClass4 == testClass1
]

{ #category : 'tests' }
RwLoadingTest >> testClassPropertyChangesWithNewClassVersion1 [
	"test class property changes that result in new class versions"

	| packageName packageNames myPackageSet className |
	packageName := 'NewClassVersionChangesTestPackage'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVersionChangeClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVersionChangeClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet
]

{ #category : 'tests' }
RwLoadingTest >> testClassPropertyChangesWithNewClassVersion2 [
	"test class property changes that result in new class versions"

	"add methods that reference the new class properties"

	| packageName packageNames myPackageSet className |
	packageName := 'NewClassVersionChangesTestPackage'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVersionChangeClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVersionChangeClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet
]

{ #category : 'tests' }
RwLoadingTest >> testClassPropertyChangesWithNoNewClassVersion [
	"test class property changes that do no result in new class versions"

	| packageName packageNames myPackageSet className |
	packageName := 'ClassPropertyChangesTestPackage'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classPropertyChangeClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classPropertyChangeClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet
]

{ #category : 'tests' }
RwLoadingTest >> testClassVarChanges [

	"characterize class var changes with respect to new version creation"

	"as of GemStone 3.4, adding and removing Class Vars does not cause the 
		creation of a new class version"

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 cvs1 cvs2 cvs3 cvs4 |
	packageName := 'ClassVarChangesTestPackage'.
	className := 'TestClassVarClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVarChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	cvs1 := testClass1 _classVars copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVarChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	cvs2 := testClass2 _classVars copy.
	self assert: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVarChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	cvs3 := testClass3 _classVars copy.
	self assert: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self classVarChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	cvs4 := testClass4 _classVars copy.
	self assert: testClass4 == testClass1
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
						classMethodDefinitions: (self combinedClassMethodsForClass1: className))
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
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
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
						classMethodDefinitions: (self combinedClassMethodsForClass2: className))
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
RwLoadingTest >> testInstVarChanges [

	"characterize class inst var changes with respect to new version creation"

	"as of GemStone 3.4, .... any change to the inst vars creates a new class version"

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 ivs1 ivs2 ivs3 ivs4 |
	packageName := 'InstVarChangesTestPackage'.
	className := 'TestInstVarClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self instVarChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	ivs1 := testClass1 _instVarNames copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self instVarChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	ivs2 := testClass2 _instVarNames copy.
	self deny: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self instVarChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	ivs3 := testClass3 _instVarNames copy.
	self deny: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions: (self instVarChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	ivs4 := testClass4 _instVarNames copy.
	self deny: testClass4 == testClass1
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
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
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
	testClass := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
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
RwLoadingTest >> testPoolDictionaryChanges [

	"characterize pool dictionary changes with respect to new version creation"

	"as of GemStone 3.4, .... "

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 pds1 pds2 pds3 pds4 |
	packageName := 'TestPoolDictionaryTestPackage'.
	className := 'TestPoolDictionaryClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	pds1 := testClass1 _poolDictionaries copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	pds2 := testClass2 _poolDictionaries copy.
	self deny: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	pds3 := testClass3 _poolDictionaries copy.
	self deny: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	pds4 := testClass4 _poolDictionaries copy.
	self deny: testClass4 == testClass1
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

{ #category : 'tests' }
RwLoadingTest >> testSingleClassVersionChange1 [

	"add an instance variable to a class that has a single method"

	"No automatic instance migration"

	| packageName packageNames myPackageSet className testClass1 testInstance1 testClass2 testInstance2 |
	packageName := 'SingleClassVersionChangeTestPackage'.
	className := 'TestSingleClassNewVersions'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions1: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance1 := testClass1 new.
	self assert: testInstance1 foo = 'foo'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions2: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance2 := testClass2 new.
	self deny: testClass2 == testClass1.
	self assert: testInstance2 foo = 'foo'
]

{ #category : 'tests' }
RwLoadingTest >> testSingleClassVersionChange2 [

	"add a class instance variable to a class that has a single method and change the method"

	"No automatic instance migration"

	| packageName packageNames myPackageSet className testClass1 testInstance1 testClass2 testInstance2 |
	packageName := 'SingleClassVersionChangeTestPackage'.
	className := 'TestSingleClassNewVersions'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions2: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance1 := testClass1 new.
	self assert: testInstance1 foo = 'foo'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions3: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance2 := testClass2 new.
	self deny: testClass2 == testClass1.
	self assert: testInstance2 foo = 'bar'
]

{ #category : 'tests' }
RwLoadingTest >> testSingleClassVersionChange3 [

	"add a class instance variable to a class that has a single method and change the protocol of the method"

	"No automatic instance migration"

	| packageName packageNames myPackageSet className testClass1 testInstance1 testClass2 testInstance2 |
	packageName := 'SingleClassVersionChangeTestPackage'.
	className := 'TestSingleClassNewVersions'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions2: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	testClass1 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance1 := testClass1 new.
	self assert: testInstance1 foo = 'foo'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions4: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := RwGsLoadedSymbolDictClass resolveClassNamed: className asSymbol.
	testInstance2 := testClass2 new.
	self deny: testClass2 == testClass1.
	self assert: testInstance2 foo = 'foo'
]
