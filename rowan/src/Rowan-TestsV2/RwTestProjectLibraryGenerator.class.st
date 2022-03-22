"
| commitComment |
RwRowanSample9Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

commitComment := ''.
RwTestProjectLibraryGenerator new
	projectName: 'RowanSample9';
	projectUrl: 'git@github.com:dalehenrich/RowanSample9.git';
	preserveRowanSHA: false;
	primeIndexCardMap;
	preserveChangesOnGithub: false;
	genSpec_0000: commitComment;
	genSpec_0001: commitComment;
	genSpec_0002: commitComment;
	genSpec_0003: commitComment;
	genSpec_0004: commitComment;
	genSpec_0005: commitComment;
	genSpec_0006: commitComment;
	genSpec_0007: commitComment;
	genSpec_0008: commitComment;
	genSpec_0009: commitComment;
	genSpec_0010: commitComment;
	genSpec_0011: commitComment;
	genSpec_0012: commitComment;
	genSpec_0013: commitComment;
	genSpec_0014: commitComment;
	genSpec_0015: commitComment;
	genSpec_0016: commitComment;
	genSpec_0017: commitComment;
	genSpec_0018: commitComment;
	genSpec_0019: commitComment;
	genSpec_0020: commitComment;
	genSpec_0021: commitComment;
	genSpec_0022: commitComment;
	genSpec_0023: commitComment;
	genSpec_0024: commitComment;
	genSpec_0025: commitComment;
	genSpec_0026: commitComment;
	genSpec_0027: commitComment;
	genSpec_0028: commitComment;
	genSpec_0029: commitComment;
	genSpec_0030: commitComment;
	genSpec_0031: commitComment;
	genSpec_0032: commitComment;
	genSpec_0033: commitComment;
	genSpec_0034: commitComment;
	genSpec_0035: commitComment;
	genSpec_0036: commitComment;
	genSpec_0037: commitComment;
	genSpec_0038: commitComment;
	genSpec_0039: commitComment;
	genSpec_0040: commitComment;
	genSpec_0041: commitComment;
	genSpec_0042: commitComment;
	genSpec_0043: commitComment;
	genSpec_0044: commitComment;
	genSpec_0045: commitComment;
	genSpec_0046: commitComment;
	genSpec_0047: commitComment;
	genSpec_0048: commitComment;
	genSpec_0049: commitComment;
	genSpec_0050: commitComment;
	genSpec_0051: commitComment;
	genSpec_0052: commitComment;
	genSpec_0053: commitComment;
	genSpec_0054: commitComment;
	genSpec_0055: commitComment;
	genSpec_0056: commitComment;
	genSpec_0057: commitComment;
	genSpec_0058: commitComment;
	genSpec_0059: commitComment;
	genSpec_0060: commitComment;
	genSpec_0064: commitComment;
	genSpec_0079: commitComment;
	genSpec_0080: commitComment;
	genSpec_0081: commitComment;
	yourself
"
Class {
	#name : 'RwTestProjectLibraryGenerator',
	#superclass : 'Object',
	#instVars : [
		'projectsHome',
		'projectName',
		'projectUrl',
		'preserveChangesOnGithub',
		'indexCardMap',
		'preserveRowanSHA'
	],
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0000: resolvedProject [
	resolvedProject
		comment: 'spec_0000 definitions';
		addNewComponentNamed: 'Core'
			comment: 'Primary component used for loading the core classes.';
		addNewComponentNamed: 'Tests'
			toComponentNamed: 'Core'
			condition: {'tests'}
			comment: 'Primary component used for loading the test classes.';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0001: resolvedProject [
	^ self _addDefinitionsFor: resolvedProject projectName: projectName
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0002: resolvedProject [
	"RowanSample4 basic package and class structure"

	| packageName1 packageName2 packageName3 packageName4 packageName5 className gemstoneClassName |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	resolvedProject
		addPackagesNamed: {packageName1.
				packageName2.
				packageName3}
			toComponentNamed: 'Core';
		addPackagesNamed: {packageName4.
				packageName5}
			toComponentNamed: 'tests/Tests';
		yourself.

	className := projectName , 'Class1'.
	((resolvedProject packageNamed: packageName1)
		addClassNamed: className
		super: 'Object'
		instvars: #()
		category: packageName1
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName2)
		addClassExtensionNamed: className)
		addClassMethod: 'bar ^''bar''' protocol: '*' , packageName2 asLowercase;
		yourself.
	gemstoneClassName := projectName , 'IdentityKeyValueDictionary'.
	((resolvedProject packageNamed: packageName3)
		addClassNamed: gemstoneClassName
		super: 'IdentityKeyValueDictionary'
		instvars: #()
		category: packageName3
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		addClassMethod: 'bar ^''bar''' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName4)
		addClassNamed: projectName , 'GemStoneTestCase'
		super: 'TestCase'
		category: packageName4
		comment: 'I test the GemStone class')
		addInstanceMethod: 'test1  self assert: ' , gemstoneClassName , ' new foo = 1'
			protocol: 'tests';
		addInstanceMethod: 'test2  self assert: ' , gemstoneClassName , ' bar = ''bar'''
			protocol: 'tests';
		yourself.
	((resolvedProject packageNamed: packageName5)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName5
		comment: 'I test the Core class')
		addInstanceMethod: 'test1  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		addInstanceMethod: 'test2  self assert: ' , className , ' bar = ''bar'''
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0003: resolvedProject [
	"primer for https://github.com/dalehenrich/Rowan/issues/230"

	| packageName1  className  |
	packageName1 := projectName , '-' , 'Core'.
	className := projectName , 'Class1'.
	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className)
		gs_options: #(#'instancesInvariant');
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0004_b: resolvedProject [
	resolvedProject
		addNewComponentNamed: 'Core3'
		comment:
			'contains ' , projectName
				,
					'-Core1 package, which is a duplicate reference, since the Core1 component also references the '
				, projectName , '-Core1 package'.
	(resolvedProject componentNamed: 'Core3') addComponentNamed: 'Core1'.
	resolvedProject
		addPackageNamed: projectName , '-Core1'
		toComponentNamed: 'Core3' yourself.
	^ self _addPackageCore1DefinitionsFor_0004: resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0004: resolvedProject [
	| className packageName userId |
	userId := resolvedProject _gemstoneAllUsersName.
	resolvedProject
		addNewComponentNamed: 'Core1'
			comment:  'contains ' , projectName , '-Core1 package';
		addNewComponentNamed: 'Tests'
			toComponentNamed: 'Core1'
			condition: {'tests'}
			comment: 'contains ' , projectName , '-Tests package';
		addNewComponentNamed: 'Core2'
			comment: 'contains ' , projectName , '-Core2 package';
		yourself.
	(resolvedProject componentNamed: 'Core1')
		addComponentNamed: 'Core2'.
	(resolvedProject componentNamed: 'Core2')
		addComponentNamed: 'Core1'.
	resolvedProject
		addPackageNamed: projectName , '-Core1'
			toComponentNamed: 'Core1'
			gemstoneDefaultSymbolDictionaryForUser:
				userId -> RwAbstractV2Test _sampleSymbolDictionaryName2;
		addPackageNamed: projectName , '-Core2'
			toComponentNamed: 'Core2';
		gemstoneSetSymbolDictName: RwAbstractV2Test _sampleSymbolDictionaryName3 
			forPackageNamed: projectName , '-Core2';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'tests/Tests';
		gemstoneSetDefaultSymbolDictNameForUser: userId
			to: RwAbstractV2Test _sampleSymbolDictionaryName1;
		yourself.
	self _addPackageCore1DefinitionsFor_0004: resolvedProject.
	packageName := projectName , '-Core2'.
	className := projectName , 'Class2'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-Tests'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0005: resolvedProject [

	| packageName1 packageName2 packageName3  className1 className2 |
	packageName1 := projectName , '-' , 'Core1'.
	packageName2 := projectName , '-' , 'Core2'.
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	resolvedProject
		addPackagesNamed: {packageName1.
				packageName2}
			toComponentNamed: 'Core';
		addPackagesNamed: {packageName3}
			toComponentNamed: 'tests/Tests';
		yourself.

	resolvedProject
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName3 
			forPackageNamed: packageName2;
		yourself.

	((resolvedProject packageNamed: packageName1)
		addClassNamed: className1
		super: 'Object'
		instvars: #()
		category: packageName1
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		addClassMethod: 'bar ^''bar''' protocol: 'accessing';
		gs_options: #(#'instancesInvariant');
		yourself.
	((resolvedProject packageNamed: packageName2)
		addClassNamed: className2
		super: 'IdentityKeyValueDictionary'
		instvars: #()
		category: packageName2
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		addClassMethod: 'bar ^''bar''' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName3)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName3
		comment: 'I test the Core class')
		addInstanceMethod: 'test1  self assert: ' , className1 , ' new foo = 1'
			protocol: 'tests';
		addInstanceMethod: 'test2  self assert: ' , className1 , ' bar = ''bar'''
			protocol: 'tests';
		addInstanceMethod: 'test3  self assert: ' , className2 , ' new foo = 1'
			protocol: 'tests';
		addInstanceMethod: 'test4  self assert: ' , className2 , ' bar = ''bar'''
			protocol: 'tests';
		addInstanceMethod: 'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName2, ''') includesKey: ''', className1 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName3, ''') includesKey: ''', className2 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', projectName , 'TestCase'' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0006: resolvedProject [

	| className1 className2 |
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	resolvedProject
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'Core';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'Extensions';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName3 
			forPackageNamed: projectName , '-' , 'GemStone';
		yourself.

	((resolvedProject packageNamed: projectName , '-' , 'Tests')
		classDefinitionNamed: projectName , 'TestCase')
		addInstanceMethod: 'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName2, ''') includesKey: ''', className1 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName3, ''') includesKey: ''', className2 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', projectName , 'TestCase'' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0007: resolvedProject [
	| packageName1 className1 |
	packageName1 := projectName , '-' , 'Core1'.
	className1 := projectName , 'Class1'.

	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		instVarNames: #('iv1');
		gs_options: #();
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0008: resolvedProject [

	| className1 className2 |
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	resolvedProject
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName3 
			forPackageNamed: projectName , '-' , 'Core';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName3
			forPackageNamed: projectName , '-' , 'Extensions';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

	((resolvedProject packageNamed: projectName , '-' , 'Tests')
		classDefinitionNamed: projectName , 'TestCase')
		updateInstanceMethod: 'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName3, ''') includesKey: ''', className1 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName2, ''') includesKey: ''', className2 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', projectName , 'TestCase'' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName2, ''') includesKey: ''', projectName , 'GemStoneTestCase'' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0009: resolvedProject [

	| className1 className2 |
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	resolvedProject
		gemstoneSetSymbolDictName: nil 
			forPackageNamed: projectName , '-' , 'Core';
		gemstoneSetSymbolDictName: nil 
			forPackageNamed: projectName , '-' , 'Extensions';
		gemstoneSetSymbolDictName: nil 
			forPackageNamed: projectName , '-' , 'Tests';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

	((resolvedProject packageNamed: projectName , '-' , 'Tests')
		classDefinitionNamed: projectName , 'TestCase')
		updateInstanceMethod: 'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', className1 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName2, ''') includesKey: ''', className2 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', projectName , 'TestCase'' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName2, ''') includesKey: ''', projectName , 'GemStoneTestCase'' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0010: resolvedProject [
	| className1 className2 |
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo:
			RwRowanSample9Test _sampleSymbolDictionaryName4.

	((resolvedProject packageNamed: projectName , '-' , 'Tests')
		classDefinitionNamed: projectName , 'TestCase')
		updateInstanceMethod:
				'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName4 , ''') includesKey: '''
						, className1
						,
							''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName2 , ''') includesKey: '''
						, className2
						,
							''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName4 , ''') includesKey: '''
						, projectName
						,
							'TestCase'' asSymbol).
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName2 , ''') includesKey: '''
						, projectName
						,
							'GemStoneTestCase'' asSymbol).
'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0011: resolvedProject [
	"Add an extension package for a method in class1 ... load everything into a single symbol dict"

	"https://github.com/GemTalk/Rowan/issues/495"

	| packageName1 packageName2 packageName3 className1 className2  |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	resolvedProject
		addPackagesNamed:
				{
				packageName2.
				}
			toComponentNamed: 'Core';
		yourself.
	((resolvedProject packageNamed: packageName2)
		addClassExtensionNamed: className1)
		addClassMethod: 'bar ^''bar''' protocol: '*' , packageName2 asLowercase;
		yourself.
	((resolvedProject packageNamed: packageName3)
		classDefinitionNamed:className2)
		addInstanceMethod: 'test2  self assert: ' , className1 , ' bar = ''bar'''
			protocol: 'tests';
		addInstanceMethod: 'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', className1 , ''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: ''' , RwRowanSample9Test _sampleSymbolDictionaryName1, ''') includesKey: ''', className2 , ''' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0012: resolvedProject [
	"Starting with spec_0011, move extension method to a new extension package; move class package to a 
		different symbol dict; move new extension package to a third symbol dictionary. This is an illegal combination
		(extension method and class in different symbol dictionaries) and does cause an error."

	"https://github.com/GemTalk/Rowan/issues/495"

	| packageName1 packageName2 packageName3 packageName4 className1 className2 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	packageName4 := projectName , '-' , 'Extensions1'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	resolvedProject
		addPackagesNamed: {packageName4}
			toComponentNamed: 'Core'
		yourself.

	resolvedProject
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName3 
			forPackageNamed: packageName4;
		yourself.

	((resolvedProject packageNamed: packageName2)
		classExtensionDefinitionNamed: className1)
		removeClassMethod: #'bar';
		yourself.
	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: className1)
		addClassMethod: 'bar ^''bar''' protocol: '*' , packageName4 asLowercase;
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		updateInstanceMethod:
				'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName2 , ''') includesKey: '''
						, className1
						,
							''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName1 , ''') includesKey: '''
						, className2
						,
							''' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0013: resolvedProject [
	"Starting with spec_0011, move extension method to a new extension package; move class package to a 
		different symbol dict; move new extension package to the first symbol dictionary. This is an illegal combination
		(extension method and class in different symbol dictionaries) and does cause an error."

	"https://github.com/GemTalk/Rowan/issues/495"

	| packageName1 packageName2 packageName3 packageName4 className1 className2 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	packageName4 := projectName , '-' , 'Extensions1'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	resolvedProject
		addPackagesNamed: {packageName4}
			toComponentNamed: 'Core'
		yourself.

	resolvedProject
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName1 
			forPackageNamed: packageName4;
		yourself.

	((resolvedProject packageNamed: packageName2)
		classExtensionDefinitionNamed: className1)
		removeClassMethod: #'bar';
		yourself.
	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: className1)
		addClassMethod: 'bar ^''bar''' protocol: '*' , packageName4 asLowercase;
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		updateInstanceMethod:
				'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName2 , ''') includesKey: '''
						, className1
						,
							''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName1 , ''') includesKey: '''
						, className2
						,
							''' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0014: resolvedProject [
	"Starting with spec_0011, move extension method to a new extension package; move new extension package 
		to the classes symbol dictionary. This is an legal combination (extension method and class in same symbol
		dictionary) and should cause an error, but currently we get an audit error."

	"https://github.com/GemTalk/Rowan/issues/495"

	| packageName1 packageName2 packageName3 packageName4 className1 className2 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	packageName4 := projectName , '-' , 'Extensions1'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	resolvedProject
		addPackagesNamed: {packageName4}
			toComponentNamed: 'Core'
		yourself.

	resolvedProject
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName1 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName1 
			forPackageNamed: packageName4;
		yourself.

	((resolvedProject packageNamed: packageName2)
		classExtensionDefinitionNamed: className1)
		removeClassMethod: #'bar';
		yourself.
	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: className1)
		addClassMethod: 'bar ^''bar''' protocol: '*' , packageName4 asLowercase;
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		updateInstanceMethod:
				'testSymbolDicts
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName1 , ''') includesKey: '''
						, className1
						,
							''' asSymbol).
	self assert: ((Rowan image symbolDictNamed: '''
						, RwRowanSample9Test _sampleSymbolDictionaryName1 , ''') includesKey: '''
						, className2
						,
							''' asSymbol).
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0015: resolvedProject [
	"Starting with spec_0014, remove the old (now empty) package from the project as triggering a package move
	as opposed to a class move seems to trigger the bug, which implies that move package is not quite right. 

	It seems that assigning a new symbol dictionary to the OLD package ... the package that the class is moved FROM
	may trigger a bogus package move.

	The test RwRowanProjectIssuesTestV2 >> testIssue495_move_class_and_extension_method_to_new_symbol_dict
	showed that #495 wasn't fixed by the V2 implementation."

	"https://github.com/GemTalk/Rowan/issues/495"

	| packageName2 |
	packageName2 := projectName , '-' , 'Extensions'.

	resolvedProject
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0016: resolvedProject [
	"Starting with spec_0016, move an extension method from one package to another with
		no modifications"

	"Move extension method from  RowanSample9-Extensions1 to RowanSample9-Extensions."

	"https://github.com/GemTalk/Rowan/issues/557"

	| className1 packageName1 packageName2 methodDefinition |
	className1 := 'RowanSample9Class1'.
	packageName1 := projectName , '-' , 'Extensions1'.
	packageName2 := projectName , '-' , 'Extensions'.

	methodDefinition := ((resolvedProject packageNamed: packageName1)
		classExtensionDefinitionNamed: className1)
		removeClassMethod: #'bar'.

	methodDefinition protocol: '*', packageName2 asLowercase.

	resolvedProject
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName1 
		forPackageNamed: packageName2;
		yourself.

	((resolvedProject packageNamed: packageName2)
		addClassExtensionNamed: className1)
		addClassMethodDefinition: methodDefinition;
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0017: resolvedProject [
	"Starting with spec_0015, change symbol dictionary of RowanSample9-Extensions to match symbol
		dictionary of RowanSample9-Extensions1"

	"https://github.com/GemTalk/Rowan/issues/557"

	| packageName1 |
	packageName1 := projectName , '-' , 'Extensions'.

	resolvedProject
		gemstoneSetSymbolDictName:  RwRowanSample9Test _sampleSymbolDictionaryName1 
		forPackageNamed: packageName1;
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0018: resolvedProject [
	"Build a sample vast/gemstone project based on information in https://github.com/dalehenrich/tonel-demos/tree/rowan_553"

	"https://github.com/GemTalk/Rowan/issues/553"

	| componentName packageName1 packageName2 extensionClassName1 className1 className2 |
	componentName := 'Core'.
	packageName1 := projectName , '-Vast-Application'.
	packageName2 := projectName , '-Common-Core'.
	extensionClassName1 := 'TestCase'.
	className1 := 'TonelExampleTest'.
	className2 := 'TonelExampleApp'.

	resolvedProject
		packagesPath: 'source';
		packageConvention: 'Rowan';
		yourself.

	resolvedProject
		comment:
				'sample vast/gemstone project, based on project structure in https://github.com/dalehenrich/tonel-demos/tree/rowan_553';
		addNewComponentNamed: 'Core'
			comment: 'Primary component used for loading the core classes.';
		addNewComponentNamed: componentName
			toComponentNamed: componentName
			condition: {'vast'}
			comment: 'manage vast packages';
		addPackageNamed: packageName1 toComponentNamed: 'vast/' , componentName;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		yourself.

	(resolvedProject packageNamed: packageName1)
		propertyAt: 'comment'
			put:
				'This is a vast only package, so the vast package properties are required?';
		propertyAt: 'vaPrerequisites' put: #('SUnit' 'TonelWriterModel');
		propertyAt: 'vaSubApplications'
			put:
				(GsTonelOrderedDictionary new
						add: #'condition' -> true;
						add: #'#subapps' -> #('TonelExampleSubApp');
						yourself);
		yourself.

	((resolvedProject packageNamed: packageName1)
		addClassNamed: className2
		super: 'Application'
		category: 'TonelExampleApp'
		comment:
			'This is a VAST application class that is paired with the test class '
				, className1 printString , '.')
		addClassMethod:
				'_PRAGMA_TonelExampleConstants
"%%PRAGMA DECLARE
	(name: TonelExampleConstants isPool: true)
	(pool: TonelExampleConstants declarations: (
		(name: Constant1 isConstant: true valueExpression: ''1'')
		(name: Constant2 isConstant: true valueExpression: ''2'')
	
	))
	"'
			protocol: 'Not categorized';
		addClassMethod:
				'writeAppToTonel

	| writer fileSystem |

	fileSystem := (Smalltalk at: #TonelCommonFileSystem) new.
	writer := (Smalltalk at: #TonelWriter) on: fileSystem.
	writer writeInWorkingDirectoryProjectIncluding: (Array with: TonelExampleApp)'
			protocol: 'Not categorized';
		addClassMethod:
				'writeAppToTonelToGit

	| writer fileSystem |

	fileSystem := (Smalltalk at: #TonelCommonFileSystem) new.
	writer := (Smalltalk at: #TonelWriter) on: fileSystem.
	writer
		writeApplication: TonelExampleApp
		into: (CfsPath named: ''c:\Users\mpeck\Documents\Instantiations\git\tonel-demos\source\'')'
			protocol: 'Not categorized';
		yourself.

	(resolvedProject packageNamed: packageName2)
		propertyAt: 'comment'
		put:
			'This package contains the code that is common between gemstone and vast, so not appropriate to add vast package properties.'.

	((resolvedProject packageNamed: packageName2)
		addClassExtensionNamed: extensionClassName1)
		addInstanceMethod:
				'tonelExampleExtendedMethodFromRootApp
	"common extension method"
	^ ''tonelExampleExtendedMethodFromRootApp'''
			protocol: 'tonel example';
		yourself.

	((resolvedProject packageNamed: packageName2)
		addClassNamed: className1
		super: extensionClassName1
		instvars: #('instVar1' 'instVar2' 'instVar3')
		classinstvars: #('classInstVar1' 'classInstVar2' 'classInstVar3')
		classvars: #('ClassVar1' 'ClassVar2' 'ClassVar3')
		category: 'TonelExampleApp'
		comment:
			'TonelExampleTest is an application used to verify that the tonel writer and reader for VA does work')
		addClassMethod:
				'classInstVar1 ^ classInstVar1 ifNil: [ classInstVar1 := ''classInstVar1'' ]'
			protocol: 'Accessing';
		addClassMethod: 'classVar1 ^ ClassVar1 ifNil: [ ClassVar1 := ''ClassVar1'' ]'
			protocol: 'Accessing';
		addInstanceMethod: 'setUp instVar1 := ''instVar1''' protocol: 'Running';
		addInstanceMethod:
				'testClassInstVarAccess self assert: self class classInstVar1 equals: ''classInstVar1'''
			protocol: 'Testing';
		addInstanceMethod:
				'testClassVarAccess self assert: self class classVar1 equals: ''ClassVar1'''
			protocol: 'Testing';
		addInstanceMethod:
				'testExtendedMethodFromRootApp self assert: self tonelExampleExtendedMethodFromRootApp equals: ''tonelExampleExtendedMethodFromRootApp'''
			protocol: 'Testing';
		addInstanceMethod:
				'testExtendedMethodFromSubApp self assert: self tonelExampleExtendedMethodFromSubApp equals: ''tonelExampleExtendedMethodFromSubApp'''
			protocol: 'Testing';
		addInstanceMethod:
				'testExtendedMethodFromSubSubApp self assert: self tonelExampleExtendedMethodFromSubSubApp equals: ''tonelExampleExtendedMethodFromSubSubApp'''
			protocol: 'Testing';
		addInstanceMethod:
				'testInstVarAccess self assert: instVar1 equals: ''instVar1'''
			protocol: 'Testing';
		addInstanceMethod:
				'testPrivateMethod self assert: self privateMethod equals: ''privateMethod'''
			protocol: 'Testing';
		addInstanceMethod:
				'testClassDefinedInSubApp self assert: TonelExampleSubAppClass notCategorizedMethod equals: ''notCategorizedMethod'''
			protocol: 'Testing';
		addInstanceMethod:
				'testClassDefinedInSubSubApp self assert: TonelExampleSubSubAppClass notCategorizedMethod2 equals: ''notCategorizedMethod2'''
			protocol: 'Testing';
		yourself.

	(((resolvedProject packageNamed: packageName2) classDefinitionNamed: className1)
		addInstanceMethod:
			'privateMethod
	"this method has a VAST-specific property, but the source is shared"
	^''privateMethod'''
		protocol: 'Accessing') propertyAt: 'vaVisibility' put: 'private'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0019: resolvedProject [
	"Conditional components test case"

	"two new conditional/nested components with a package per
		add an extension method for packageName2"

	"https://github.com/GemTalk/Rowan/issues/554"

	| packageName1 packageName2 componentName componentName1 componentName2 conditionArray groupName |
	packageName1 := projectName , '-' , 'Extensions1'.
	packageName2 := projectName , '-' , 'Extensions'.
	componentName := 'Core'.
	componentName1 := 'subs/Core1'.
	componentName2 := 'subs/Core2'.
	conditionArray := {'common'}.
	groupName := 'core'.

	resolvedProject
		addSimpleNestedComponentNamed: componentName1
			condition: 'common'
			comment: '';
		addSimpleNestedComponentNamed: componentName2
			condition: 'common'
			comment: '';
		gemstoneSetSymbolDictName: nil forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: nil forPackageNamed: packageName2;
		removePackageNamed: packageName1
			fromComponentNamed: componentName;
		removePackageNamed: packageName2
			fromComponentNamed: componentName;
		addPackageNamed: packageName1
			toComponentNamed: componentName1;
		addPackageNamed: packageName2
			toComponentNamed: componentName2;
		addComponentNamed: componentName1
			toComponentNamed: componentName;
		addComponentNamed: componentName2
			toComponentNamed: componentName;
		yourself.

	((resolvedProject packageNamed: packageName2)
		addClassExtensionNamed: projectName , 'Class1')
		addInstanceMethod: 'baz ^''baz''' protocol: '*' , packageName2 asLowercase;
		yourself.
	((resolvedProject packageNamed: projectName , '-Tests')
		classDefinitionNamed: projectName , 'TestCase')
		addInstanceMethod:
				'test3 self assert: ' , projectName , 'Class1 new baz = ''baz'''
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0020: resolvedProject [
	"Start with spec_0019, and undo work done to recreate spec_0017 structure -- find and fix holes in api"

	"https://github.com/GemTalk/Rowan/issues/554"

	| packageName1 packageName2 componentName componentName1 componentName2 conditionArray groupName |
	packageName1 := projectName , '-' , 'Extensions1'.
	packageName2 := projectName , '-' , 'Extensions'.
	componentName := 'Core'.
	componentName1 := 'subs/Core1'.
	componentName2 := 'subs/Core2'.
	conditionArray := {'common'}.
	groupName := 'core'.

	resolvedProject
		removeComponentNamed: componentName1;
		removeComponentNamed: componentName2;
		addPackageNamed: packageName1
			toComponentNamed: componentName;
		addPackageNamed: packageName2
			toComponentNamed: componentName;
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName1
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName1
			forPackageNamed: packageName2;
		yourself.


	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0021: resolvedProject [
	"Start with spec_0011, add pre and post load doits plus tests to validate that post load doits had been executed (part 1). Preload doit validation will be in part 2."

	"https://github.com/GemTalk/Rowan/issues/568"

	| packageName1 packageName2 packageName3 className1 className2 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		classVarNames: #('PostLoadDoit' 'PreLoadDoit');
		addClassMethod: 'postLoadDoit ^PostLoadDoit' protocol: 'accessing';
		addClassMethod: 'preLoadDoit ^PreLoadDoit' protocol: 'accessing';
		addClassMethod: 'postLoadDoit: value PostLoadDoit := value'
			protocol: 'accessing';
		addClassMethod: 'preLoadDoit: value PreLoadDoit := value'
			protocol: 'accessing';
		yourself.

	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		addInstanceMethod:
				'testPostLoadDoits
	"part 1"
	self assert: ' , className1
						,
							' preLoadDoit = nil.
	self assert: ' , className1
						,
							' postLoadDoit = ''done''.
'
			protocol: 'tests';
		yourself.

	resolvedProject
		addPreloadDoitName: 'doits/preloadDoit'
			withSource: '(Rowan globalNamed: ', className1 printString, ') ifNotNil: [:class | class perform: #preLoadDoit: with: ''done'' ]'
			toComponentNamed: 'Core';
		addPostloadDoitName: 'doits/postloadDoit'
			withSource: className1 , ' postLoadDoit: ''done'''
			toComponentNamed: 'Core';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0022: resolvedProject [
	"Start with spec_0021, add pre and post load doits adjust tests to validate that pre load doits had been executed (part 2)"

	"https://github.com/GemTalk/Rowan/issues/568"

	| packageName3 className1 className2 |
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		updateInstanceMethod:
				'testPostLoadDoits
	"part 2"
	self assert: ' , className1
						,
							' preLoadDoit = ''done''.
	self assert: ' , className1
						,
							' postLoadDoit = ''done''.
'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0023: resolvedProject [
	"Start with spec_0011, add a sequence of packages each subclassing off of the the other to be loaded in order."

	"https://github.com/GemTalk/Rowan/issues/568"

	| packageName1 packageName2 packageName3 packageName4 testPackageName1 testPackageName2 testPackageName3 testPackageName4 className1 className2 className3 className4 testClassName1 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Core2'.
	packageName3 := projectName , '-' , 'Core3'.
	packageName4 := projectName , '-' , 'Core4'.
	testPackageName1 := projectName , '-' , 'Tests'.
	testPackageName2 := projectName , '-' , 'Tests2'.
	testPackageName3 := projectName , '-' , 'Tests3'.
	testPackageName4 := projectName , '-' , 'Tests4'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'Class2'.
	className3 := projectName , 'Class3'.
	className4 := projectName , 'Class4'.
	testClassName1 := projectName , 'TestCase'.

	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		classInstVarNames: #('postLoadDoit' 'preLoadDoit');
		addClassMethod: 'postLoadDoit ^postLoadDoit' protocol: 'accessing';
		addClassMethod: 'preLoadDoit ^preLoadDoit' protocol: 'accessing';
		addClassMethod: 'postLoadDoit: value postLoadDoit := value'
			protocol: 'accessing';
		addClassMethod: 'preLoadDoit: value preLoadDoit := value'
			protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: testPackageName1)
		classDefinitionNamed: testClassName1)
		removeInstanceMethod: #'test2';
		addInstanceMethod:
				'testPostLoadDoits
	self assert: ' , className1
						,
							' postLoadDoit = 1.
'
			protocol: 'tests';
		yourself.

	{packageName2.
	packageName3.
	packageName4.
	testPackageName2.
	testPackageName3.
	testPackageName4}
		do: [ :packageName | resolvedProject addRawPackageNamed: packageName ].

	((resolvedProject packageNamed: packageName2)
		addClassNamed: className2
		super: className1
		instvars: #()
		category: packageName2
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^2' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: testPackageName2)
		addClassExtensionNamed: testClassName1)
		addInstanceMethod:
				'testPostLoadDoits2
	self assert: ' , className1
						,
							' preLoadDoit = 2.
	self assert: ' , className1
						,
							' postLoadDoit = 1.
	self assert: ' , className2
						,
							' postLoadDoit = 2.
'
			protocol: '*' , testPackageName2 asLowercase;
		yourself.

	((resolvedProject packageNamed: packageName3)
		addClassNamed: className3
		super: className2
		instvars: #()
		category: packageName3
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^3' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: testPackageName3)
		addClassExtensionNamed: testClassName1)
		addInstanceMethod:
				'testPostLoadDoits3
	self assert: ' , className1
						,
							' preLoadDoit = 2.
	self assert: ' , className1
						,
							' postLoadDoit = 1.
	self assert: ' , className2
						,
							' preLoadDoit = 3.
	self assert: ' , className2
						,
							' postLoadDoit = 2.
	self assert: ' , className3
						,
							' postLoadDoit = 3.
'
			protocol: '*' , testPackageName3 asLowercase;
		yourself.

	((resolvedProject packageNamed: packageName4)
		addClassNamed: className4
		super: className3
		instvars: #()
		category: packageName4
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^4' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: testPackageName4)
		addClassExtensionNamed: testClassName1)
		addInstanceMethod:
				'testPostLoadDoits4
	self assert: ' , className1
						,
							' preLoadDoit = 2.
	self assert: ' , className1
						,
							' postLoadDoit = 1.
	self assert: ' , className2
						,
							' preLoadDoit = 3.
	self assert: ' , className2
						,
							' postLoadDoit = 2.
	self assert: ' , className3
						,
							' preLoadDoit = 4.
	self assert: ' , className3
						,
							' postLoadDoit = 3.
	self assert: ' , className4
						,
							' preLoadDoit = nil.
	self assert: ' , className4
						,
							' postLoadDoit = 4.
'
			protocol: '*' , testPackageName4 asLowercase;
		yourself.

	resolvedProject removeComponentNamed: 'Core'.
	resolvedProject removeComponentNamed: 'tests/Tests'.
	resolvedProject
		addNewComponentNamed: 'Core'
		condition: 'common'
		comment:
			'master component, that will load 4 simple components in order with each component doing a complete, independent load'.

	resolvedProject
		addNewComponentNamed: 'Core1'
			condition: 'common'
			comment: 'First component for loading first package';
		addPackagesNamed:
				{packageName1.
					testPackageName1}
			toComponentNamed: 'Core1';
		addNewComponentNamed: 'Core2'
			condition: 'common'
			comment: 'Second component for loading second package';
		addPackagesNamed:
				{packageName2.
					testPackageName2}
			toComponentNamed: 'Core2';
		addNewComponentNamed: 'Core3'
			condition: 'common'
			comment: 'Third component for loading third package';
		addPackagesNamed:
				{packageName3.
					testPackageName3}
			toComponentNamed: 'Core3';
		addNewComponentNamed: 'Core4'
			condition: 'common'
			comment: 'Fourth component for loading fourth package';
		addPackagesNamed:
				{packageName4.
					testPackageName4}
			toComponentNamed: 'Core4';
		yourself.

	(resolvedProject componentNamed: 'Core')
		addComponentNamed: 'Core1';
		addComponentNamed: 'Core2';
		addComponentNamed: 'Core3';
		addComponentNamed: 'Core4';
		yourself.

	resolvedProject
		addPostloadDoitName: 'doits/postloadDoit_Core1'
			withSource: className1 , ' postLoadDoit: 1'
			toComponentNamed: 'Core1';
		addPreloadDoitName: 'doits/preloadDoit_Core2'
			withSource: className1 , ' preLoadDoit: 2'
			toComponentNamed: 'Core2';
		addPostloadDoitName: 'doits/postloadDoit_Core2'
			withSource: className2 , ' postLoadDoit: 2'
			toComponentNamed: 'Core2';
		addPreloadDoitName: 'doits/preloadDoit_Core3'
			withSource: className2 , ' preLoadDoit: 3'
			toComponentNamed: 'Core3';
		addPostloadDoitName: 'doits/postloadDoit_Core3'
			withSource: className3 , ' postLoadDoit: 3'
			toComponentNamed: 'Core3';
		addPreloadDoitName: 'doits/preloadDoit_Core4'
			withSource: className3 , ' preLoadDoit: 4'
			toComponentNamed: 'Core4';
		addPostloadDoitName: 'doits/postloadDoit_Core4'
			withSource: className4 , ' postLoadDoit: 4'
			toComponentNamed: 'Core4';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0024: resolvedProject [
	"Start with  spec_0011, add pharo, gemstone, and vast conditional packages using
		RwSimpleProjectLoadComponentV2, intended to form the basis for working out the
		details necessary to enable pharo, gemstone and vast to share common code."

	"https://github.com/GemTalk/Rowan/issues/553"

	| packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 className1 className2 className3 className4 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.

	packageName4 := projectName , '-' , 'Export-GemStone'.
	packageName5 := projectName , '-' , 'Export-Pharo'.
	packageName6 := projectName , '-' , 'Export-VAST'.
	packageName7 := projectName , '-' , 'VAST-Application'.
	packageName8 := projectName , '-' , 'VAST-Test-Application'.

	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.
	className3 := projectName , 'ApplicationClass1'.
	className4 := projectName , 'TestApplication1'.

	{packageName4.
	packageName5.
	packageName6.
	packageName7.
	packageName8}
		do: [ :packageName | resolvedProject addRawPackageNamed: packageName ].

	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		addInstanceMethod: 'ivar1 ^ ivar1' protocol: 'accessing';
		addInstanceMethod: 'ivar1: anObject  ivar1 := anObject' protocol: 'accessing';
		yourself.

	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		addInstanceMethod:
				'testExport
	"exexute without error"
	| filePath object |
	filePath := (FileReference newTempFilePrefix: ''' , projectName
						,
							'_export'' suffix: ''ston'') pathString.
	object := ' , className1
						,
							' new ivar1: filePath.
	object exportTo: filePath'
			protocol: 'tests';
		yourself.

	((resolvedProject packageNamed: packageName4)
		addClassExtensionNamed: className1)
		addInstanceMethod:
				'exportTo: pathString
	| stream |
	stream := GsFile openWriteOnServer: pathString.
	[STON put: self onStreamPretty: stream]
		ensure: [ stream close ].'
			protocol: '*' , packageName4 asLowercase;
		yourself.

	((resolvedProject packageNamed: packageName5)
		addClassExtensionNamed: className1)
		addInstanceMethod:
				'exportTo: pathString
	pathString asFileReference writeStreamDo: [:stream |
		STON put: self onStreamPretty: stream].'
			protocol: '*' , packageName5 asLowercase;
		yourself.

	((resolvedProject packageNamed: packageName6)
		addClassExtensionNamed: className1)
		addInstanceMethod:
				'exportTo: pathString
	| outStream |
	outStream := CfsWriteFileStream openEmpty: pathString.
	[STON put: self onStreamPretty: stream]
		ensure: [ stream close ].'
			protocol: 'accessing';
		yourself.

	((resolvedProject packageNamed: packageName7)
		addClassNamed: className3
		super: 'Application'
		instvars: #()
		category: packageName7
		comment: 'I am a VAST appliation class')
		addClassMethod:
				'writeAppToTonel
	| writer fileSystem |
	fileSystem := TonelCommonFileSystem new.
	writer := TonelWriter on: fileSystem.
	writer writeInWorkingDirectoryProjectIncluding: (Array with: ' , className3
						, ')'
			protocol: 'accessing';
		yourself.

	((resolvedProject packageNamed: packageName8)
		addClassNamed: className4
		super: 'Application'
		instvars: #()
		category: packageName8
		comment: 'I am a VAST test ppliation class')
		addClassMethod:
				'writeAppToTonel
	| writer fileSystem |
	fileSystem := TonelCommonFileSystem new.
	writer := TonelWriter on: fileSystem.
	writer writeInWorkingDirectoryProjectIncluding: (Array with: ' , className4
						, ')'
			protocol: 'accessing';
		yourself.

	resolvedProject
		removeComponentNamed: 'Core';
		removeComponentNamed: 'tests/Tests';
		addNewComponentNamed: 'Core'
			condition: 'common'
			comment: 'Top-level component for loading the application';
		addPlatformComponentNamed: 'Core'
			toComponentNamed: 'Core'
			pathNameArray:
				{'platform'.
				'gemstone'}
			conditionPathArray:
				{'common'.
				{'gemstone'}}
			comment: 'Component for loading gemstone-specific packages';
		addPlatformComponentNamed: 'Core'
			toComponentNamed: 'Core'
			pathNameArray:
				{'platform'.
				'pharo'}
			conditionPathArray:
				{'common'.
				{'pharo'}}
			comment: 'Component for loading pharo-specific packages';
		addPlatformComponentNamed: 'Core'
			toComponentNamed: 'Core'
			pathNameArray:
				{'platform'.
				'vast'}
			conditionPathArray:
				{'common'.
				{'vast'}}
			comment:
				'Component for loading vast-specific test packages and components. 
		The component named VAST_IMAGE/STON is a pseudo-component that refers to 
		applications expected to reside in the VAST image.

		The package named RowanSample9-VAST-Application, which contains the Envy 
		Application class is also included in this component.';
		addPlatformComponentNamed: 'Tests'
			toComponentNamed: 'Core'
			pathNameArray:
				{'tests'.
				'platform'.
				'vast'}
			conditionPathArray:
				{'test'.
				'common'.
				{'vast'}}
			comment:
				'Component for loading vast-specific test packages and components. 
		The component named VAST_IMAGE/SUnit is a pseudo-component that refers to 
		applications expected to reside in the VAST image';
		yourself.

	resolvedProject
		addPackagesNamed:
				{packageName1.
					packageName2}
			toComponentNamed: 'Core';
		addPackageNamed: packageName3 toComponentNamed: 'tests/Tests';
		addPackageNamed: packageName4 toComponentNamed: 'platform/gemstone/Core';
		addPackageNamed: packageName5 toComponentNamed: 'platform/pharo/Core';
		addPackagesNamed:
				{packageName6.
					packageName7}
			toComponentNamed: 'platform/vast/Core';
		addPackageNamed: packageName8 toComponentNamed: 'tests/platform/vast/Tests';
		yourself.

false ifTrue: [ 
	resolvedProject
		addComponentNamed: 'VAST_IMAGE/STON' toComponentNamed: 'platform/vast/Core';
		addComponentNamed: 'VAST_IMAGE/Sunit'
			toComponentNamed: 'tests/platform/vastCore';
		yourself.	"add the VAST_IMAGE application dependencies"
].
	resolvedProject
		_validate:
				{'common'.
					'gemstone'};
		_validate:
				{'common'.
					'pharo'};
		_validate:
				{'common'.
					'vast'};
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0025: resolvedProject_0025 [
	"Start with  spec_0001, add 3 embedded projects"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpecification_0025 packageNames |
	loadSpecification_0025 := resolvedProject_0025 _loadSpecification.
	packageNames := resolvedProject_0025 packageNames.

	loadSpecification_0025 customConditionalAttributes: {'tests'}.

	resolvedProject_0025
		removeComponentNamed: 'Core';
		addNewComponentNamed: 'Core' condition: 'common' comment: 'the component';
		addComponentNamed: 'tests/Tests' toComponentNamed: 'Core';
		yourself.
	packageNames
		do: [ :packageName | 
			packageName ~= (projectName , '-Tests')
				ifTrue: [ resolvedProject_0025 addPackageNamed: packageName toComponentNamed: 'Core' ] ].

	1 to: 3 do: [ :index | 
		| resolvedProjectName suffix loadSpecification specName resolvedProject baseName |
		suffix := '_' , index asString.
		specName := loadSpecification_0025 specName , suffix.
		resolvedProjectName := resolvedProject_0025 name , suffix.

		loadSpecification := RwEmbeddedLoadSpecificationV2 new
			projectName: resolvedProjectName;
			projectAlias: resolvedProject_0025 name;
			projectsHome: self projectsHome;
			gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
			yourself.

		baseName := 'rowan' , suffix.
		loadSpecification
			specName: resolvedProjectName;
			revision: loadSpecification_0025 revision;
			gitUrl: self projectUrl;
			componentNames: {'Core'};
			customConditionalAttributes: {'tests'};
			comment: loadSpecification_0025 comment;
			yourself.

		resolvedProject := loadSpecification resolve.

		resolvedProject
			addNewComponentNamed: 'Core' condition: 'common' comment: 'the component';
			addNewComponentNamed: 'tests/Tests'
				condition: 'tests'
				comment: 'the test component';
			yourself.

		resolvedProject projectSpecification
			componentsPath: baseName , '/components';
			packagesPath: baseName , '/src';
			projectsPath: baseName , '/projects';
			specsPath: baseName , '/specs';
			projectSpecPath: baseName;
			yourself.

		self
			_addSimpleDefinitionsFor: resolvedProject
			projectName: resolvedProject projectName.

		resolvedProject projectRoots
			do: [ :rootDir | 
				"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
				rootDir ensureDeleteAll ].

		resolvedProject
			export;
			exportLoadSpecification.	"write the rowan directory structure"

		resolvedProject_0025
			addProjectNamed: resolvedProjectName
			toComponentNamed: 'Core'.

		resolvedProject_0025 projectsRoot ensureCreateDirectory.
		resolvedProject specsRoot / resolvedProjectName , 'ston'
			copyTo: resolvedProject_0025 projectsRoot / resolvedProjectName , 'ston' ].

	^ resolvedProject_0025
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0026: resolvedProject [
	"Start with  spec_0000, create a project using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"start with a single component, single package project."

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentName1 packageName1 className1 testClassName1 |
	resolvedProject removeComponentNamed: 'Core'.

	componentName1 := projectName.
	packageName1 := projectName , '-Core'.
	className1 := projectName , 'Class1'.
	testClassName1 := projectName , 'TestCase1'.

	resolvedProject 
		specComponentNames: {componentName1};
		yourself.

	resolvedProject addNewComponentNamed: componentName1.

	resolvedProject
		addPackageNamed: packageName1 toComponentNamed: componentName1;
		yourself.

	((resolvedProject packageNamed: packageName1)
		addClassNamed: className1
		super: 'Object'
		instvars: #('ivar1')
		category: packageName1
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName1)
		addClassNamed: testClassName1
		super: 'TestCase'
		category: packageName1
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className1 , ' new foo = 1'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0027: resolvedProject [
	"Start with  spec_0026, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"split pacakges into core and tests and use move class api to move class definitions from package to package"

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentName1 packageName1 className1 testClassName1 packageName2 |
	componentName1 := projectName.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	className1 := projectName , 'Class1'.
	testClassName1 := projectName , 'TestCase1'.

	resolvedProject
		addPackageNamed: packageName2 toComponentNamed: componentName1;
		yourself.

	(resolvedProject packageNamed: packageName1)
		moveClassNamed: testClassName1
		modifyClassDefinition: [ :classDef | classDef category: packageName2 ]
		toPackage: (resolvedProject packageNamed: packageName2) yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0028: resolvedProject [
	"Start with  spec_0026, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"add conditional Tests component"

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentPath1 componentName2 packageName1 className1 testClassName1 packageName2 componentPath2  |
	componentPath1 := projectName.
	componentName2 :=  projectName.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	className1 := projectName , 'Class1'.
	testClassName1 := projectName , 'TestCase1'.

	componentPath2 := resolvedProject
		addNewComponentNamed: componentName2
			toComponentNamed: componentPath1
			condition: #('common' 'tests' ).

	resolvedProject customConditionalAttributes: #('tests').

	resolvedProject 
		movePackageNamed: packageName2 toComponentNamed: componentPath2;
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0029: resolvedProject [
	"Start with  spec_0028, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"add 2 more classes, tests and packages (one class/package) and create category components: Core, Definitions, Specs, Tests"

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentPath1 componentPath2 componentPath3 componentName3 componentName4 componentName5 componentName6 packageName1 className1 className2 className3 testClassName1 testClassName2 testClassName3 packageName2 packageName3 packageName4 packageName5 packageName6 componentName2 deletedComponentName |
	deletedComponentName := 'common/RowanSample9'.
	componentPath1 := projectName.
	componentPath2 := 'common/tests/' , projectName.
	componentName2 := 'Tests'.
	componentName3 := 'Core'.
	componentName4 := 'Definitions'.
	componentName5 := 'Specs'.
	componentName6 := 'Tests'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	packageName3 := projectName , '-' , componentName4.
	packageName4 := projectName , '-' , componentName4 , '-Tests'.
	packageName5 := projectName , '-' , componentName5.
	packageName6 := projectName , '-' , componentName5 , '-Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'Class2'.
	className3 := projectName , 'Class3'.
	testClassName1 := projectName , 'TestCase1'.
	testClassName2 := projectName , 'TestCase2'.
	testClassName3 := projectName , 'TestCase3'.

	resolvedProject removeComponentNamed: deletedComponentName.

	componentPath2 := resolvedProject
		renameComponentNamed: componentPath2
		to: componentName2.

	componentPath3 := resolvedProject
		addNewComponentNamed: componentName3
		toComponentNamed: componentPath1
		condition: #('common').
	resolvedProject
		movePackageNamed: packageName1 toComponentNamed: componentPath3;
		yourself.

	componentPath3 := resolvedProject
		addNewComponentNamed: componentName4
		toComponentNamed: componentPath1
		condition: #('common').
	resolvedProject
		addPackageNamed: packageName3 toComponentNamed: componentPath3;
		yourself.
	resolvedProject
		addPackageNamed: packageName4 toComponentNamed: componentPath2;
		yourself.

	componentPath3 := resolvedProject
		addNewComponentNamed: componentName5
		toComponentNamed: componentPath1
		condition: #('common').
	resolvedProject
		addPackageNamed: packageName5 toComponentNamed: componentPath3;
		yourself.
	resolvedProject
		addPackageNamed: packageName6 toComponentNamed: componentPath2;
		yourself.

	componentPath3 := resolvedProject
		addNewComponentNamed: componentName6
		toComponentNamed: componentPath1
		condition: #('common').
	(resolvedProject componentNamed: componentPath1)
		removeComponentNamed: componentPath2;
		addComponentNamed: componentPath3;
		yourself.
	(resolvedProject componentNamed: componentPath3)
		addComponentNamed: componentPath2;
		yourself.


	self _spec_0029_classCreationBlock
		value: resolvedProject
		value: className2
		value: packageName3
		value: testClassName2
		value: packageName4.

	self _spec_0029_classCreationBlock
		value: resolvedProject
		value: className3
		value: packageName5
		value: testClassName3
		value: packageName6.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0030: resolvedProject [
	"Start with  spec_0029, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"add platform conditional extension methods and classes plus tests - gemstone pharo gs3.5-"

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentName4 componentName5 componentName6 componentPath1 componentPath2 componentPath3 componentPath4 componentPath5 packageName1 className1 className2 className3 testClassName1 testClassName2 testClassName3 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 packageName9 packageName10 componentPath |
	componentPath1 := projectName.
	componentPath2 := 'common/tests/Tests'.
	componentPath3 := 'common/Core'.
	componentPath4 := 'common/Definitions'.
	componentPath5 := 'common/Specs'.
	componentName4 := 'Definitions'.
	componentName5 := 'Specs'.
	componentName6 := 'Tests'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	packageName3 := projectName , '-' , componentName4.
	packageName4 := projectName , '-' , componentName4 , '-Tests'.
	packageName5 := projectName , '-' , componentName5.
	packageName6 := projectName , '-' , componentName5 , '-Tests'.
	packageName7 := projectName , '-' , componentName4 , '-GemStone'.
	packageName8 := projectName , '-' , componentName4 , '-GemStone' , '-Tests'.
	packageName9 := projectName , '-' , componentName4 , '-Pharo'.
	packageName10 := projectName , '-' , componentName4 , '-Pharo' , '-Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'Class2'.
	className3 := projectName , 'Class3'.
	testClassName1 := projectName , 'TestCase1'.
	testClassName2 := projectName , 'TestCase2'.
	testClassName3 := projectName , 'TestCase3'.

	{{packageName3.
	className2.
	packageName4.
	testClassName2}}
		do: [ :ar | 
			((resolvedProject packageNamed: (ar at: 1)) classDefinitionNamed: (ar at: 2))
				addInstanceMethod:
						'bar ^ (self class _dictionaryClass new) at: self foo put: 1; yourself'
					protocol: 'accessing';
				yourself.
			((resolvedProject packageNamed: (ar at: 3)) classDefinitionNamed: (ar at: 4))
				addInstanceMethod:
						'testBar | inst | inst := ' , (ar at: 2)
								, ' new. self assert: (inst bar at: inst foo) = 1'
					protocol: 'tests';
				addInstanceMethod:
						'testBarClass | inst | inst := ' , (ar at: 2)
								, ' new. self assert: inst bar class == self _dictionaryClass'
					protocol: 'tests';
				yourself ].

	componentPath := resolvedProject
		addNewComponentNamed: componentName4
		toComponentNamed: componentPath4
		condition: #('common' 'gemstone').
	resolvedProject
		addPackageNamed: packageName7 toComponentNamed: componentPath;
		yourself.

	componentPath := resolvedProject
		addNewComponentNamed: componentName4
		toComponentNamed: componentPath4
		condition: #('common' 'tests' 'gemstone').
	resolvedProject
		addPackageNamed: packageName8 toComponentNamed: componentPath;
		yourself.

	{{packageName7.
	className2.
	packageName8.
	testClassName2}}
		do: [ :ar | 
			((resolvedProject packageNamed: (ar at: 1)) addClassExtensionNamed: (ar at: 2))
				addClassMethod: '_dictionaryClass ^ RcKeyValueDictionary'
					protocol: '*' , (ar at: 1) asLowercase;
				yourself.
			((resolvedProject packageNamed: (ar at: 3))
				addClassExtensionNamed: (ar at: 4))
				addInstanceMethod: '_dictionaryClass ^ RcKeyValueDictionary'
					protocol: '*' , (ar at: 3) asLowercase;
				yourself ].

	componentPath := resolvedProject
		addNewComponentNamed: componentName4
		toComponentNamed: componentPath4
		condition: #('common' 'pharo').
	resolvedProject
		addPackageNamed: packageName9 toComponentNamed: componentPath;
		yourself.

	componentPath := resolvedProject
		addNewComponentNamed: componentName4
		toComponentNamed: componentPath4
		condition: #('common' 'tests' 'pharo').
	resolvedProject
		addPackageNamed: packageName10 toComponentNamed: componentPath;
		yourself.

	{{packageName9.
	className2.
	packageName10.
	testClassName2}}
		do: [ :ar | 
			((resolvedProject packageNamed: (ar at: 1)) addClassExtensionNamed: (ar at: 2))
				addClassMethod: '_dictionaryClass ^ PluggableDictionary'
					protocol: '*' , (ar at: 1) asLowercase;
				yourself.
			((resolvedProject packageNamed: (ar at: 3))
				addClassExtensionNamed: (ar at: 4))
				addInstanceMethod: '_dictionaryClass ^ PluggableDictionary'
					protocol: '*' , (ar at: 3) asLowercase;
				yourself ].

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0031: resolvedProject [
	"Start with  spec_0029, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"Add platform conditional extension methods and classes plus tests - pharo, gemstone and gs3.[5-]. Use platform components with alias."

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentName4 componentName5 componentName6 componentPath1 componentPath2 componentPath3 componentPath4 componentPath5 packageName1 className1 className2 className3 testClassName1 testClassName2 testClassName3 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 packageName9 packageName10 componentPath |
	componentPath1 := projectName.
	componentPath2 := 'common/tests/Tests'.
	componentPath3 := 'common/Core'.
	componentPath4 := 'common/Definitions'.
	componentPath5 := 'common/Specs'.
	componentName4 := 'Definitions'.
	componentName5 := 'Specs'.
	componentName6 := 'Tests'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	packageName3 := projectName , '-' , componentName4.
	packageName4 := projectName , '-' , componentName4 , '-Tests'.
	packageName5 := projectName , '-' , componentName5.
	packageName6 := projectName , '-' , componentName5 , '-Tests'.
	packageName7 := projectName , '-' , componentName4 , '-GemStone'.
	packageName8 := projectName , '-' , componentName4 , '-GemStone' , '-Tests'.
	packageName9 := projectName , '-' , componentName4 , '-Pharo'.
	packageName10 := projectName , '-' , componentName4 , '-Pharo' , '-Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'Class2'.
	className3 := projectName , 'Class3'.
	testClassName1 := projectName , 'TestCase1'.
	testClassName2 := projectName , 'TestCase2'.
	testClassName3 := projectName , 'TestCase3'.

	{{packageName3.
	className2.
	packageName4.
	testClassName2}}
		do: [ :ar | 
			((resolvedProject packageNamed: (ar at: 1)) classDefinitionNamed: (ar at: 2))
				addInstanceMethod:
						'bar ^ (self class _dictionaryClass new) at: self foo put: 1; yourself'
					protocol: 'accessing';
				yourself.
			((resolvedProject packageNamed: (ar at: 3)) classDefinitionNamed: (ar at: 4))
				addInstanceMethod:
						'testBar | inst | inst := ' , (ar at: 2)
								, ' new. self assert: (inst bar at: inst foo) = 1'
					protocol: 'tests';
				addInstanceMethod:
						'testBarClass | inst | inst := ' , (ar at: 2)
								, ' new. self assert: inst bar class == self _dictionaryClass'
					protocol: 'tests';
				yourself ].

	componentPath := resolvedProject
		addPlatformComponentNamed: componentName4
		toComponentNamed: componentPath4
		pathNameArray:
			{'common'.
			'_gemstone_glass'}
		conditionPathArray:
			{'common'.
			{'gemstone'.
			'glass'}}.
	resolvedProject
		addPackageNamed: packageName7 toComponentNamed: componentPath;
		yourself.

	componentPath := resolvedProject
		addPlatformComponentNamed: componentName4
		toComponentNamed: componentPath4
		pathNameArray:
			{'common'.
			'tests'.
			'_gemstone_glass'}
		conditionPathArray:
			{'common'.
			'tests'.
			{'gemstone'.
			'glass'}}.
	resolvedProject
		addPackageNamed: packageName8 toComponentNamed: componentPath;
		yourself.

	{{packageName7.
	className2.
	packageName8.
	testClassName2}}
		do: [ :ar | 
			((resolvedProject packageNamed: (ar at: 1)) addClassExtensionNamed: (ar at: 2))
				addClassMethod: '_dictionaryClass ^ RcKeyValueDictionary'
					protocol: '*' , (ar at: 1) asLowercase;
				yourself.
			((resolvedProject packageNamed: (ar at: 3))
				addClassExtensionNamed: (ar at: 4))
				addInstanceMethod: '_dictionaryClass ^ RcKeyValueDictionary'
					protocol: '*' , (ar at: 3) asLowercase;
				yourself ].

	componentPath := resolvedProject
		addPlatformComponentNamed: componentName4
		toComponentNamed: componentPath4
		pathNameArray:
			{'common'.
			'_pharo'}
		conditionPathArray:
			{'common'.
			{'pharo'}}.
	resolvedProject
		addPackageNamed: packageName9 toComponentNamed: componentPath;
		yourself.

	componentPath := resolvedProject
		addPlatformComponentNamed: componentName4
		toComponentNamed: componentPath4
		pathNameArray:
			{'common'.
			'tests'.
			'_pharo'}
		conditionPathArray:
			{'common'.
			'tests'.
			{'pharo'}}.
	resolvedProject
		addPackageNamed: packageName10 toComponentNamed: componentPath;
		yourself.

	{{packageName9.
	className2.
	packageName10.
	testClassName2}}
		do: [ :ar | 
			((resolvedProject packageNamed: (ar at: 1)) addClassExtensionNamed: (ar at: 2))
				addClassMethod: '_dictionaryClass ^ PluggableDictionary'
					protocol: '*' , (ar at: 1) asLowercase;
				yourself.
			((resolvedProject packageNamed: (ar at: 3))
				addClassExtensionNamed: (ar at: 4))
				addInstanceMethod: '_dictionaryClass ^ PluggableDictionary'
					protocol: '*' , (ar at: 3) asLowercase;
				yourself ].

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0032: resolvedProject [
	"Start with  spec_0031, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"Explore the use of shared directory for sharing code between two conditions, like v1 and v2 ... first create v1 and v2 packages (that can be independently loaded or loaded together in GemStone and use the class in shared component for code that is common to both -- not controlled by attributes"

	"https://github.com/GemTalk/Rowan/issues/573"

	| componentName4 componentName5 componentName6 componentName7 componentPath1 componentPath2 componentPath3 componentPath4 componentPath5 componentPath6 packageName1 className1 className2 className3 className4 className5 className6 testClassName1 testClassName2 testClassName3 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 packageName9 packageName10 packageName11 packageName12 packageName13 packageName14 packageName15 componentPath |
	componentPath1 := projectName.
	componentPath2 := 'common/tests/Tests'.
	componentPath3 := 'common/Core'.
	componentPath4 := 'common/Definitions'.
	componentPath5 := 'common/Specs'.
	componentPath6 := 'common/Tests'.
	componentName4 := 'Definitions'.
	componentName5 := 'Specs'.
	componentName6 := 'Tests'.
	componentName7 := 'shared/Specs'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	packageName3 := projectName , '-' , componentName4.
	packageName4 := projectName , '-' , componentName4 , '-Tests'.
	packageName5 := projectName , '-' , componentName5.
	packageName6 := projectName , '-' , componentName5 , '-Tests'.
	packageName7 := projectName , '-' , componentName4 , '-GemStone'.
	packageName8 := projectName , '-' , componentName4 , '-GemStone' , '-Tests'.
	packageName9 := projectName , '-' , componentName4 , '-Pharo'.
	packageName10 := projectName , '-' , componentName4 , '-Pharo' , '-Tests'.
	packageName11 := projectName , '-' , componentName5 , '-Shared'.
	packageName12 := projectName , '-' , componentName5 , '-V1'.
	packageName13 := projectName , '-' , componentName5 , '-V2'.
	packageName14 := projectName , '-' , componentName5 , '-V1-Tests'.
	packageName15 := projectName , '-' , componentName5 , '-V2-Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'Class2'.
	className3 := projectName , 'Class3'.
	className4 := projectName , componentName5 , 'SharedClass4'.
	className5 := projectName , componentName5 , 'v1Class5'.
	className6 := projectName , componentName5 , 'v2Class5'.
	testClassName1 := projectName , 'TestCase1'.
	testClassName2 := projectName , 'TestCase2'.
	testClassName3 := projectName , 'TestCase3'.

	resolvedProject customConditionalAttributes: #('tests' 'v1' 'v2').

	resolvedProject
		addNewNestedComponentNamed: componentName7
		comment:
			'This component is intended for sharing common code and should not be loaded independently'.
	resolvedProject
		addPackageNamed: packageName11 toComponentNamed: componentName7;
		yourself.

	componentPath := resolvedProject
		addNewComponentNamed: componentName5
		toComponentNamed: componentPath5
		condition: #('common' 'v1').
	resolvedProject
		addPackageNamed: packageName12 toComponentNamed: componentPath;
		addComponentNamed: componentName7 toComponentNamed: componentPath;
		yourself.
	componentPath := resolvedProject
		addNewComponentNamed: componentName5
		toComponentNamed: componentPath5
		condition: #('common' 'v2').
	resolvedProject
		addPackageNamed: packageName13 toComponentNamed: componentPath;
		addComponentNamed: componentName7 toComponentNamed: componentPath;
		yourself.

	componentPath := resolvedProject
		addNewComponentNamed: componentName5
		toComponentNamed: componentPath6
		condition: #('common' 'tests' 'v1').
	resolvedProject
		addPackageNamed: packageName14 toComponentNamed: componentPath;
		yourself.
	componentPath := resolvedProject
		addNewComponentNamed: componentName5
		toComponentNamed: componentPath6
		condition: #('common' 'tests' 'v2').
	resolvedProject
		addPackageNamed: packageName15 toComponentNamed: componentPath;
		yourself.

	((resolvedProject packageNamed: packageName11)
		addClassNamed: className4
		super: className3
		instvars: #()
		category: packageName11
		comment: 'I am an example class' yourself)
		addInstanceMethod: 'shared ^ ''shared''' protocol: 'accessing';
		yourself.
	(resolvedProject packageNamed: packageName12)
		addClassNamed: className5
		super: className4
		instvars: #()
		category: packageName12
		comment: 'I am an example class' yourself.
	(resolvedProject packageNamed: packageName13)
		addClassNamed: className6
		super: className4
		instvars: #()
		category: packageName13
		comment: 'I am an example class' yourself.

	((resolvedProject packageNamed: packageName14)
		addClassExtensionNamed: testClassName3)
		addInstanceMethod:
			'testV1 self assert: (' , className5 , ' new) shared = ''shared'''
		protocol: '*' , packageName14 asLowercase.

	((resolvedProject packageNamed: packageName15)
		addClassExtensionNamed: testClassName3)
		addInstanceMethod:
			'testV2 self assert: (' , className6 , ' new) shared = ''shared'''
		protocol: '*' , packageName15 asLowercase.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0033: resolvedProject [
	"Start with  spec_0011, update class definition to match RowanSample1 branch issue_345, i.e., add constraints to a class - porting RwRowanSample1Test>>testIssue345 and RwRowanSample1Test>>testIssue514 to v2"

	"https://github.com/GemTalk/Rowan/issues/573"

	| packageName1 packageName2 packageName3 className1 className2  |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed:className1)
		instVarNames: #('aaa' 'bbb' 'ccc');
		 gs_constraints: { {'aaa' . 'String'} . {'bbb' . 'Integer'} };
		yourself.

	((resolvedProject packageNamed: packageName3)
		classDefinitionNamed:className2)
		addInstanceMethod: 'testConstraints
	self assert: (', className1, ' _constraintsEqual: { { #''aaa'' . String } . { #''bbb'' . Integer }})'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0034: resolvedProject [
	"Start with spec_0001, remove the instance variable. Foundation for instance migration testing - _migration_0."

	| packageName1 className1 |
	packageName1 := projectName , '-' , 'Core'.
	className1 := projectName , 'Class1'.

	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed:className1)
			instVarNames: #().

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0035: resolvedProject [
	"Start with spec_0034, add class hierarchy with instance variables and methods. _migration_1."

	| packageName1 packageDef |
	packageName1 := projectName , '-' , 'Core'.
	packageDef := resolvedProject packageNamed: packageName1.
	{#('Object' 'RowanSample9Class' #('ivar0')).
	#('RowanSample9Class' 'RowanSample9ClassA' #('a' 'ivar1')).
	#('RowanSample9ClassA' 'RowanSample9ClassC' #('c' 'ivar2')).
	#('RowanSample9ClassA' 'RowanSample9ClassD' #('d' 'ivar2')).
	#('RowanSample9Class' 'RowanSample9ClassB' #('b' 'ivar1')).
	#('RowanSample9ClassB' 'RowanSample9ClassE' #('e' 'ivar2')).
	#('RowanSample9ClassB' 'RowanSample9ClassF' #('f' 'ivar2'))}
		do: [ :ar | 
			| superClassName className instvars classDef |
			superClassName := ar at: 1.
			className := ar at: 2.
			instvars := ar at: 3.
			classDef := packageDef
				classDefinitionNamed: className
				ifAbsent: [ 
					RwClassDefinition
						newForClassNamed: className
						super: superClassName
						category: packageName1 ].
			classDef instVarNames: instvars.
			packageDef updateClassDefinition: classDef.
			instvars
				do: [ :iv | 
					classDef
						updateInstanceMethod: iv , ' ^ ' , iv protocol: 'accessing';
						updateInstanceMethod: iv , ': obj ' , iv , ' := obj'
							protocol: 'accessing';
						yourself ] ].

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0036: resolvedProject [
	"Start with spec_0035, move ivs ivar1 and ivar2 up to the top of the hierarcy; methods should be okay. _migration_2."

	| packageName1 packageDef |
	packageName1 := projectName , '-' , 'Core'.
	packageDef := resolvedProject packageNamed: packageName1.
	{#('Object' 'RowanSample9Class' #('ivar0' 'ivar1' 'ivar2') #()).
	#('RowanSample9Class' 'RowanSample9ClassA' #('a') #(#'ivar1' #'ivar2')).
	#('RowanSample9ClassA' 'RowanSample9ClassC' #('c') #(#'ivar1' #'ivar2')).
	#('RowanSample9ClassA' 'RowanSample9ClassD' #('d') #(#'ivar1' #'ivar2')).
	#('RowanSample9Class' 'RowanSample9ClassB' #('b') #(#'ivar1' #'ivar2')).
	#('RowanSample9ClassB' 'RowanSample9ClassE' #('e') #(#'ivar1' #'ivar2')).
	#('RowanSample9ClassB' 'RowanSample9ClassF' #('f') #(#'ivar1' #'ivar2'))}
		do: [ :ar | 
			| superClassName className instvars classDef removeSelectors |
			superClassName := ar at: 1.
			className := ar at: 2.
			instvars := ar at: 3.
			removeSelectors := ar at: 4.
			classDef := packageDef
				classDefinitionNamed: className
				ifAbsent: [ 
					RwClassDefinition
						newForClassNamed: className
						super: superClassName
						category: packageName1 ].
			classDef instVarNames: instvars.
			packageDef updateClassDefinition: classDef.
			instvars
				do: [ :iv | 
					classDef
						updateInstanceMethod: iv , ' ^ ' , iv protocol: 'accessing';
						updateInstanceMethod: iv , ': obj ' , iv , ' := obj'
							protocol: 'accessing';
						yourself ].
			removeSelectors
				do: [ :sel | 
					classDef removeInstanceMethod: sel ifAbsent: [  ].
					classDef removeInstanceMethod: (sel , ':') asSymbol ifAbsent: [  ] ] ].

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0037: resolvedProject [
	"Start with spec_0002, add constraints to RowanSample9Class1 and RowanSample9IdentityKeyValueDictionary, plus tests to validate the constraints"

	| packageName1 packageName2 packageName3 packageName4 packageName5 className1 className2 className3 className4 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.

	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed:className1)
		instVarNames: #( 'instvar3' 'instvar1' 'instvar2' 'instvar4');
		 gs_constraints: { {'instvar1' . 'Integer'} . {'instvar2' . 'Array'} . {'instvar3' . 'Boolean'} . {'instvar4' . 'Array'} };
		yourself.

	((resolvedProject packageNamed: packageName3)
		classDefinitionNamed: className2)
		 gs_constraints: { 'String' };
		yourself.

	((resolvedProject packageNamed: packageName4)
		classDefinitionNamed: className4)
		addInstanceMethod: 'testConstraints
	| x |
	self assert: (x := ', className2, ' _varyingConstraint) = String.
'
			protocol: 'tests';
		yourself.

	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: className3)
		addInstanceMethod: 'testConstraints
	| x |
	self assert: (x := ', className1, ' _constraintOn: #instvar1) = Integer.
	self assert: (x := ', className1, ' _constraintOn: #instvar2) = Array.
	self assert: (x := ', className1, ' _constraintOn: #instvar3) = Boolean.
	self assert: (x := ', className1, ' _constraintOn: #instvar4) = Array.
'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0038: resolvedProject [
	"Start with spec_0002, add new class in new package. spec_0002 = issue_185_0, spec_0038 = issue_185_1"

	"
		In RowanSample4 issue_185_0, the GemStone packages are in a separate symbol dictionary from the core.
		In RowanSample4 'issue_185_1, the NewRowanSample4 class is in the same symbol dictionary as the GemStone packages.
	"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	resolvedProject
		addPackageNamed: packageName6 toComponentNamed: componentName;
		yourself.

	((resolvedProject packageNamed: packageName6)
		addClassNamed: className5
		super: 'Object'
		instvars: #()
		category: packageName6
		comment: 'I am a new class')
		addInstanceMethod: 'foo ^5' protocol: 'accessing';
		yourself.

	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: className3)
		addInstanceMethod: 'test', className5, '
	| x |
	self assert: (x := ', className5, ' new foo) = 5.
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0039: resolvedProject [
	"Start with spec_0038, add class var to NewRowanSample9Class (issue_185_7)"

	"
		In RowanSample4 issue_185_7, the NewRowanSample4 class has a new class var.
	"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	((resolvedProject packageNamed: packageName6)
		classDefinitionNamed: className5)
		classVarNames: #('CVar1');
		addClassMethod: 'cVar ^ CVar1' protocol: 'accessing';
		addClassMethod: 'initialize CVar1 := 1' protocol: 'initialization';
		yourself.

	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: className3)
		addInstanceMethod: 'testClassVars_', className5, '
	| x |
	self assert: (x := ', className5, ' cVar) = 1.
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0040: resolvedProject [
	"Start with spec_0038, add inst var to NewRowanSample9Class (create new class version)(issue_185_5)"

	"
		In RowanSample4 issue_185_5, the NewRowanSample4 class has a new inst var.
	"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	((resolvedProject packageNamed: packageName6)
		classDefinitionNamed: className5)
		instVarNames: #('ivar1');
		addInstanceMethod: 'ivar1 ^ ivar1' protocol: 'accessing';
		addInstanceMethod: 'initialize ivar1 := 1' protocol: 'initialization';
		addClassMethod: 'new ^ (self basicNew) initialize; yourself' protocol: 'instance creation';
		yourself.

	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: className3)
		addInstanceMethod: 'testInstVars_', className5, '
	| x |
	self assert: (x := ', className5, ' new ivar1) = 1.
'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0041: resolvedProject [
	"Start with spec_0038, rename package to RowanSample9-RenamedPackage (issue_185_2)"

	"
		In RowanSample4 issue_185_2, rename package.
	"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 packageName7 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	resolvedProject renamePackageNamed: packageName6 to: packageName7.

	^ resolvedProject

]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0042: resolvedProject [
	"Start with spec_0039, rename package to RowanSample9-RenamedPackage (issue_185_8)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 packageName7 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	resolvedProject renamePackageNamed: packageName6 to: packageName7.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0043: resolvedProject [
	"Start with spec_0040, rename package to RowanSample9-RenamedPackage (issue_185_6)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 className1 className2 className3 className4 className5 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	resolvedProject renamePackageNamed: packageName6 to: packageName7.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0044: resolvedProject [
	"Start with spec_0038, rename package to RowanSample9-RenamedPackage (issue_185_4)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 className1 className2 className3 className4 className5 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	resolvedProject renamePackageNamed: packageName6 to: packageName7.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0045: resolvedProject [
	"Start with spec_0038, change a comment in the component (benign commit)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 className1 className2 className3 className4 className5 component |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New' , projectName , 'Class'.

	component := resolvedProject componentNamed: componentName.
	component comment: 'https://github.com/dalehenrich/Rowan/issues/284 ... benign commit, loaded commit id should be updated'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0046: resolvedProject [
	"Start with spec_0041, add inst vars to NewRowanSample9Class and rename the rest of the packages (add _295) to package name (issue_295_2)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 packageName7 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	((resolvedProject packageNamed: packageName7)
		classDefinitionNamed: className5)
		instVarNames: #('ivar2' 'ivar3');
		addInstanceMethod: 'ivar2 ^ ivar2' protocol: 'accessing';
		addInstanceMethod: 'initialize ivar2 := 2' protocol: 'initialization';
		addClassMethod: 'new ^ (self basicNew) initialize; yourself' protocol: 'instance creation';
		yourself.

	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: className3)
		addInstanceMethod: 'testInstVars_', className5, '
	| x |
	self assert: (x := ', className5, ' new ivar2) = 2.
'
			protocol: 'tests';
		yourself.

	resolvedProject 
		renamePackageNamed: packageName1 to: packageName1, '_295';
		renamePackageNamed: packageName2 to: packageName2, '_295';
		renamePackageNamed: packageName3 to: packageName3, '_295';
		renamePackageNamed: packageName4 to: packageName4, '_295';
		renamePackageNamed: packageName5 to: packageName5, '_295';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0047: resolvedProject [
	"Start with spec_0040, move RowanSample9IdentityKeyValueDictionary to -Core package; add subclass of NewRowanSample9Class in New package  (issue_295_4)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 className6 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.
	className6 := 'New', projectName , 'Subclass'.

	resolvedProject moveClassNamed: className2 toPackageNamed: packageName1.

	(resolvedProject packageNamed: packageName6)
		addClassNamed: className6
		super: className5
		instvars: #()
		category: packageName6
		comment: 'I am a new subclass';
		yourself.

	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: className3)
		addInstanceMethod: 'testInstVars_', className6, '
	| x |
	self assert: (x := ', className6, ' new ivar1) = 1.
'
			protocol: 'tests';
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0048: resolvedProject [
	"Start with spec_0046; add subclass of NewRowanSample9Class in Renamed package (issue_295_5); for issue #304, make Renamed package conditional on renamed attribute"

	| componentName1 componentName2 componentName3 packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 packageName7 className6 packageName8 componentPath |
	componentName1 := 'Core'.
	componentName2 := 'Rename'.
	componentName3 := 'Tests'.
	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.
	className6 := 'New', projectName , 'Subclass'.

"Issue #304 support: add conditional component structure"
		(resolvedProject componentNamed: componentName1)
		removePackageNamed: packageName7;
		yourself.

	componentPath := resolvedProject
		addNewComponentNamed: componentName2 
			toComponentNamed: componentName1 
			condition: {'renamed'}.
	(resolvedProject componentNamed: componentPath)
		addPackageNames: {packageName7}.

	componentPath := resolvedProject
		addNewComponentNamed: componentName3 
			toComponentNamed: componentName1 
			condition: {'renamed' . 'tests'}.
	(resolvedProject componentNamed: componentPath)
		addPackageNames: {packageName8}.

"update load spec"
	resolvedProject customConditionalAttributes:  (resolvedProject customConditionalAttributes,  { 'renamed'}) asSet asArray.

"add definitions"
	(resolvedProject packageNamed: packageName7)
		addClassNamed: className6
		super: className5
		instvars: #()
		category: packageName7
		comment: 'I am a new subclass';
		yourself.

	((resolvedProject addPackageNamed: packageName8)
		addClassExtensionNamed: className3)
		addInstanceMethod: 'testInstVars_', className6, '
	| x |
	self assert: (x := ', className6, ' new ivar2) = 2.
'
			protocol: '*', packageName8 asLowercase;
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0049: resolvedProject [
	"Start with spec_0048; create an additional load spec without the renamed atribute"

	| componentName1 componentName2 componentName3 packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 packageName7 className6 packageName8 loadSpec |
	componentName1 := 'Core'.
	componentName2 := 'Rename'.
	componentName3 := 'Tests'.
	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.
	className6 := 'New', projectName , 'Subclass'.

	loadSpec := resolvedProject loadSpecification copy .

	loadSpec 
		specName: 'Issue_304';
		customConditionalAttributes:  { 'tests' };
		revision: 'spec_0048';
		comment: 'variant of load spec without renamed attribute (Issue #304)';
		export.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0050: resolvedProject [
	"Start with spec_0047; move RowanSample9IdentityKeyValueDictionary to RowanSample9-GemStone (issue_295_6)"

	| componentName packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className1 className2 className3 className4 className5 className6 |
	componentName := 'Core'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.
	className6 := 'New', projectName , 'Subclass'.

	resolvedProject moveClassNamed: className2 toPackageNamed: packageName3.

	resolvedProject loadSpecification
		customConditionalAttributes:  { 'tests' };
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0051: resolvedProject [
	"Start with spec_0002; add a component that includes tests as part of core attributes (issue_493)"

	| packageName1 packageName2 packageName3 packageName4 packageName5 componentName1 componentName2 component1 component2 testComponentName testComponent |
	componentName1 := 'Core'.
	componentName2 := 'Issue_493'.
	testComponentName := 'tests/Tests'.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.

	component1 := resolvedProject componentNamed: componentName1.
	component2 := resolvedProject addNewComponentNamed: componentName2.
	testComponent := resolvedProject componentNamed: testComponentName.

	component2
		addPackageNames: component1 packageNames, testComponent packageNames;
		comment: 'Single component that includes all of the packages';
		yourself.

	resolvedProject loadSpecification
		componentNames: { componentName2 };
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0052: resolvedProject [
	"Start with spec_0001; add an embedded project that is basically a copy of spec_001; and adjust class in primary project be a subclass of class in embedded project"

	| requiredProjectName requiredLoadSpec requiredProject embeddedProjectReference componentName requiredClassName1 className1 packageName1 |
	requiredProjectName := projectName , '_embedded_1'.
	embeddedProjectReference := Path * 'embeddedProjects' / requiredProjectName
		/ 'rowan'.
	componentName := 'Core'.
	requiredClassName1 :=  requiredProjectName , 'Class1'.
	className1 :=  projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	
	requiredLoadSpec := RwEmbeddedLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		projectAlias: projectName;
		revision: 'spec_0052';
		gitUrl: self projectUrl;
		componentNames: {componentName};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	requiredProject := (requiredLoadSpec resolveWithParentProject: resolvedProject) resolve.
	requiredProject projectSpecification
		specsPath: (embeddedProjectReference / 'specs') pathString;
		projectsPath: (embeddedProjectReference / 'projects') pathString;
		packagesPath: (embeddedProjectReference / 'src') pathString;
		componentsPath: (embeddedProjectReference / 'components') pathString;
		yourself.
	requiredProject
		projectSpecFile: (embeddedProjectReference / 'project' , 'ston') pathString;
		addNewComponentNamed: componentName;
		addNewComponentNamed: 'Tests'
			toComponentNamed: componentName
			condition: {'tests'};
		yourself.
	self _addDefinitionsFor: requiredProject projectName: requiredProjectName.

	requiredProject
		projectsHome: self projectsHome;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	requiredProject specsRoot / requiredLoadSpec specName , 'ston'
		copyTo: resolvedProject projectsRoot / requiredLoadSpec specName , 'ston'.

	(resolvedProject componentNamed: componentName)
		addProjectNamed: requiredProjectName;
		yourself.

	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className1)
			superclassName: requiredClassName1;
			instVarNames: #();
			yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0053: resolvedProject [
	"Start with spec_0052; add instance side method #ivar1 referencing inherited ivar to class in primary project"

	| requiredProjectName componentName requiredClassName1 className1 packageName1 className2 packageName2
		embeddedProjectReference |
	requiredProjectName := projectName , '_embedded_1'.
	embeddedProjectReference := Path * 'embeddedProjects' / requiredProjectName
		/ 'rowan'.
	componentName := 'Core'.
	requiredClassName1 :=  requiredProjectName , 'Class1'.
	className1 :=  projectName , 'Class1'.
	className2 :=  projectName , 'TestCase'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	
	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className1)
			addInstanceMethod: 'ivar1 ^ ivar1'
			protocol: 'accessing';
			yourself.

	((resolvedProject packageNamed: packageName2)
		classDefinitionNamed: className2)
		addInstanceMethod: 'testInstVars_', className1, '
	| x |
	self assert: (x := ', className1, ' new ivar1) isNil.
'
			protocol: 'tests';
		yourself.

	resolvedProject projectsRoot ensureCreateDirectory.
	(RwEmbeddedLoadSpecificationV2 fromFile: self projectsHome / projectName / embeddedProjectReference pathString / 'specs' / 'RowanSample9_embedded_1', 'ston')
		exportTo: resolvedProject projectsRoot.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0054: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0052; add platform conditional packages to the embedded project and the primary project."

	| resolvedEmbeddedProject |

	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject  condition: 'gnu' packageLabel: 'Gnu'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gnu' packageLabel: 'Gnu'.

	(resolvedEmbeddedProject := embeddedProject resolve)
		projectsHome: self projectsHome;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0055: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0052; add platform conditional packages (GemStone version 2.4.x) to the embedded project and the primary project."

	| resolvedEmbeddedProject |
	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject condition: 'gs2.[4-]' packageLabel: 'GemStone24x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gs2.[4-]'  packageLabel: 'GemStone24x'.

	(resolvedEmbeddedProject := embeddedProject resolve)
		projectsHome: self projectsHome;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0056: resolvedProject embeddedProject: embeddedProject [
	"Start with spec_0052; add platform conditional packages (GemStone version 3.6.x, 3.5.x, and 3.2.15) to the embedded project and the primary project. This is expected to reproduce issue #594 for those three patforms."

	| resolvedEmbeddedProject |
	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject condition: 'gs2.[4-]' packageLabel: 'GemStone24x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gs2.[4-]'  packageLabel: 'GemStone24x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: resolvedProject condition: 'gs3.[2-]' packageLabel: 'GemStone3x'.
	self _addPlatformConditionalDefinitionsFor_0054Project: embeddedProject condition: 'gs3.[2-]'  packageLabel: 'GemStone3x'.

	(resolvedEmbeddedProject := embeddedProject resolve)
		projectsHome: self projectsHome;
		export;
		exportLoadSpecification.	"write the embedded project directory structure"

	resolvedProject projectsRoot ensureCreateDirectory.
	resolvedEmbeddedProject specsRoot / resolvedEmbeddedProject specName , 'ston'
		copyTo: resolvedProject projectsRoot / resolvedEmbeddedProject specName , 'ston'.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0057: resolvedProject [
	"Start with spec_0000, use Rowan packageConvention; simple project with extension methods"

	| packageName1 packageName2 packageName5 className |
	self _addDefinitionsFor: resolvedProject projectName: projectName.
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName5 := projectName , '-' , 'Tests'.
	resolvedProject
		addPackagesNamed: {packageName2} toComponentNamed: 'Core';
		yourself.
	className := projectName , 'Class1'.
	((resolvedProject packageNamed: packageName2) addClassExtensionNamed: className)
		addClassMethod: 'bar ^''bar''' protocol: 'accessing';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0058: resolvedProject [
	"Start with spec_0057 and set class category to nil and empty string (https://github.com/GemTalk/Rowan/issues/169)"

	| packageName1 packageName2 packageName5 className testClassName |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName5 := projectName , '-' , 'Tests'.
	className := projectName , 'Class1'.
	testClassName := projectName , 'TestCase'.
	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className)
		category: nil;
		yourself.
	((resolvedProject packageNamed: packageName5)
		classDefinitionNamed: testClassName)
		category: '';
		addInstanceMethod:
				'testCategories
	| x |
	self assert: (x := ' , className
						,
							' category) =  ''Kernel''.
	self assert: (x := ' , testClassName , ' category) = '''''
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0059: resolvedProject [
	"Component api revamp. Start with spec_0001; add 3 conditions: alt1, alt2, alt3; one extension method and test case per condition (see https://github.com/GemTalk/Rowan/issues/653 and https://github.com/GemTalk/Rowan/issues/660)"

	| packageName1 packageName2 className1 className2 componentName1 componentName2 componentName3 componentName4 componentName5 componentName6 componentName7 componentName8 componentName9 componentName10 componentName11 componentName12 componentName13 componentName14 componentName15 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 packageName9 packageName10 packageName11 packageName12 packageName13 packageName14 |
	componentName1 := 'Core'.
	componentName2 := 'tests/Tests'.
	componentName3 := 'conditions/Core'.
	componentName4 := 'conditions/alt1/Core'.
	componentName5 := 'conditions/alt2/Core'.
	componentName6 := 'conditions/alt3/Core'.
	componentName7 := 'tests/alt1/Tests'.
	componentName8 := 'tests/alt2/Tests'.
	componentName9 := 'tests/alt3/Tests'.
	componentName10 := 'conditions/alt1/platform/platformA/Core'.
	componentName11 := 'conditions/alt1/platform/platformB/Core'.
	componentName12 := 'conditions/alt1/platform/platformAorB/Core'.
	componentName13 := 'tests/alt1/platform/platformA/Tests'.
	componentName14 := 'tests/alt1/platform/platformB/Tests'.
	componentName15 := 'tests/alt1/platform/platformAorB/Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	packageName3 := projectName , '-Core-Alt1'.
	packageName4 := projectName , '-Core-Alt2'.
	packageName5 := projectName , '-Core-Alt3'.
	packageName6 := projectName , '-Tests-Alt1'.
	packageName7 := projectName , '-Tests-Alt2'.
	packageName8 := projectName , '-Tests-Alt3'.
	packageName9 := projectName , '-Core-Alt1-PlatformA'.
	packageName10 := projectName , '-Core-Alt1-PlatformB'.
	packageName11 := projectName , '-Core-Alt1-PlatformAorB'.
	packageName12 := projectName , '-Tests-Alt1-PlatformA'.
	packageName13 := projectName , '-Tests-Alt1-PlatformB'.
	packageName14 := projectName , '-Tests-Alt1-PlatformAorB'.

	resolvedProject
		packageConvention: 'Rowan';
		yourself.

	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		category: componentName1.
	((resolvedProject packageNamed: packageName2) classDefinitionNamed: className2)
		category: componentName2.

	{'alt1'.
	'alt2'.
	'alt3'}
		do: [ :condition | 
			| componentName pkgSuffix package |
			pkgSuffix := condition copy.
			pkgSuffix at: 1 put: (condition at: 1) asUppercase.
			componentName := resolvedProject
				addComponentStructureFor: componentName1
				pathNameArray:
					{'conditions'.
					condition}
				conditionPathArray:
					{'common'.
					condition}.
			package := resolvedProject
				addPackageNamed: projectName , '-Core-' , pkgSuffix
				toComponentNamed: componentName.
			(package addClassExtensionNamed: className1)
				addInstanceMethod: condition , ' ^ ' , condition printString
				protocol: 'alternates'.


			componentName := resolvedProject
				addComponentStructureFor: 'Tests'
				startingAtComponentNamed: componentName2
				pathNameArray:
					{'tests'.
					condition}
				conditionPathArray:
					{'tests'.
					condition}.
			package := resolvedProject
				addPackageNamed: projectName , '-Tests-' , pkgSuffix
				toComponentNamed: componentName.
			(package addClassExtensionNamed: className2)
				addInstanceMethod:
					'test' , pkgSuffix , ' self assert: ' , className1 , ' new ' , condition , ' = '
						, condition printString
				protocol: 'tests' ].

	{{'platformA'.
	#('platformA')}.
	{'platformB'.
	#('platformB')}.
	{'platformAorB'.
	#('platformA' 'platformB')}.
	{'platformC'.
	#('platformC')}}
		do: [ :conditionArray | 
			| componentName pkgSuffix package |
			pkgSuffix := (conditionArray at: 1) copy.
			pkgSuffix at: 1 put: ((conditionArray at: 1) at: 1) asUppercase.
			(conditionArray at: 1) ~= 'platformC'
				ifTrue: [ 
					"platformA and platformB follow the standard pattern of extension methods and tests that the 
						extension methods were loaded"
					componentName := resolvedProject
						addComponentStructureFor: componentName1
						pathNameArray:
							{'conditions'.
							'alt1'.
							'platform'.
							(conditionArray at: 1)}
						conditionPathArray:
							{'common'.
							'alt1'.
							'common'.
							(conditionArray at: 2)}.
					package := resolvedProject
						addPackageNamed: projectName , '-Core-' , pkgSuffix
						toComponentNamed: componentName.
					(package addClassExtensionNamed: className1)
						addInstanceMethod:
							(conditionArray at: 1) , ' ^ ' , (conditionArray at: 1) printString
						protocol: 'platforms'.


					componentName := resolvedProject
						addComponentStructureFor: 'Tests'
						startingAtComponentNamed: componentName2
						pathNameArray:
							{'tests'.
							'alt1'.
							'platform'.
							(conditionArray at: 1)}
						conditionPathArray:
							{'tests'.
							'alt1'.
							'common'.
							(conditionArray at: 2)}.
					package := resolvedProject
						addPackageNamed: projectName , '-Tests-' , pkgSuffix
						toComponentNamed: componentName.
					(package addClassExtensionNamed: className2)
						addInstanceMethod:
							'test' , pkgSuffix , ' self assert: ' , className1 , ' new '
								, (conditionArray at: 1) , ' = ' , (conditionArray at: 1) printString
						protocol: 'tests' ]
				ifFalse: [ 
					"platformC will not load any extension packages for the core, but will load test extensions to confirm that 
						none of the platformA and platformB methods were loaded"
					componentName := resolvedProject
						addComponentStructureFor: 'Tests'
						startingAtComponentNamed: componentName2
						pathNameArray:
							{'tests'.
							'alt1'.
							'platform'.
							(conditionArray at: 1)}
						conditionPathArray:
							{'tests'.
							'alt1'.
							'common'.
							(conditionArray at: 2)}.
					package := resolvedProject
						addPackageNamed: projectName , '-Tests-' , pkgSuffix
						toComponentNamed: componentName.
					(package addClassExtensionNamed: className2)
						addInstanceMethod:
							'test' , pkgSuffix
								,
									' | selectors |
	selectors := ' , className1
								,
									' selectors.
	#(platformA platformB platformAorB) do: [:sel | self deny: (selectors includes: sel) ]'
						protocol: 'tests' ] ].

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0060: resolvedProject [
	"Start with spec_0001; add RowanSample10 as an external project ... based on genSpec_0052"

	| requiredProjectName requiredLoadSpec componentName requiredClassName1 className1 packageName1 |
	requiredProjectName := 'RowanSample10'.
	componentName := 'Core'.
	requiredClassName1 :=  requiredProjectName , 'Class1'.
	className1 :=  projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredProjectName;
		projectName: requiredProjectName;
		revision: 'spec_0001';
		gitUrl: 'https://git@github.com:dalehenrich/RowanSample10';
		componentNames: {componentName};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedProject projectsRoot ensureCreateDirectory.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.

	(resolvedProject componentNamed: componentName)
		addProjectNamed: requiredProjectName;
		yourself.

	((resolvedProject packageNamed: packageName1)
		classDefinitionNamed: className1)
			superclassName: requiredClassName1;
			instVarNames: #();
			yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0064: resolvedProject [
	"Start with spec_0060; WITHOUT RowanSample10 as an external project ... and add some packages that duplicate RowanSample10 package names and content ... in concert with a reload of spec_0060 reproduce https://github.com/GemTalk/Rowan/issues/680"

	| requiredProjectName packageName className componentName |
	requiredProjectName := 'RowanSample10'.
	packageName := requiredProjectName , '-Core'.
	className := requiredProjectName , 'Class1'.
componentName := 'Core'.

	resolvedProject
		addPackagesNamed: {packageName} toComponentNamed: componentName;
		yourself.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.

	(resolvedProject componentNamed: componentName)
		removeProjectNamed: requiredProjectName;
		yourself.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0079: resolvedProject [
	"Start with spec_0002; and write in filetree format instead of tonel format."

	"https://github.com/GemTalk/Rowan/issues/761"

	resolvedProject	packageFormat: 'filetree'.
self halt.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0080: resolvedProject [
	"Start with spec_0002; read and write in Pharo tonel format v1.0 (keys and values are Symbols)."

	"https://github.com/GemTalk/Rowan/issues/777"

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0081: resolvedProject [
	"Start with spec_0011; add 2 more instance methods and 3 class methods to RowanSample9Class1 in RowanSample9-Core; then add 3 instance-side extension methods to RowanSample9Class1 and 2 more class-side extension methods; add tests for all of the new methods. Fodder for audit testing."

	| packageName1 packageName2 packageName3 className1 className2 |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'TestCase'.

	((resolvedProject packageNamed: packageName1) classDefinitionNamed: className1)
		addInstanceMethod: 'foo2 ^2' protocol: 'accessing';
		addInstanceMethod: 'foo3 ^3' protocol: 'accessing';
		addClassMethod: 'bar1 ^1' protocol: 'accessing';
		addClassMethod: 'bar2 ^2' protocol: 'accessing';
		addClassMethod: 'bar3 ^3' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: packageName2)
		classExtensionDefinitionNamed: className1)
		addInstanceMethod: 'foo4 ^4' protocol: '*' , packageName2 asLowercase;
		addInstanceMethod: 'foo5 ^5' protocol: '*' , packageName2 asLowercase;
		addInstanceMethod: 'foo6 ^6' protocol: '*' , packageName2 asLowercase;
		addClassMethod: 'bar4 ^4' protocol: '*' , packageName2 asLowercase;
		addClassMethod: 'bar5 ^5' protocol: '*' , packageName2 asLowercase;
		yourself.
	((resolvedProject packageNamed: packageName3) classDefinitionNamed: className2)
		addInstanceMethod: 'test3  self assert: ' , className1 , ' new foo2 = 2'
			protocol: 'tests';
		addInstanceMethod: 'test4  self assert: ' , className1 , ' new foo3 = 3'
			protocol: 'tests';
		addInstanceMethod: 'test5  self assert: ' , className1 , ' new foo4 = 4'
			protocol: 'tests';
		addInstanceMethod: 'test6  self assert: ' , className1 , ' new foo5 = 5'
			protocol: 'tests';
		addInstanceMethod: 'test7  self assert: ' , className1 , ' new foo6 = 6'
			protocol: 'tests';
		addInstanceMethod: 'test8  self assert: ' , className1 , ' bar1 = 1'
			protocol: 'tests';
		addInstanceMethod: 'test9  self assert: ' , className1 , ' bar2 = 2'
			protocol: 'tests';
		addInstanceMethod: 'test10  self assert: ' , className1 , ' bar3 = 3'
			protocol: 'tests';
		addInstanceMethod: 'test11  self assert: ' , className1 , ' bar4 = 4'
			protocol: 'tests';
		addInstanceMethod: 'test12  self assert: ' , className1 , ' bar5 = 5'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor: resolvedProject projectName: aProjectName [
	| className packageName |
	resolvedProject
		addPackagesNamed: {(aProjectName , '-Core')}
			toComponentNamed: 'Core';
		addPackageNamed: aProjectName , '-Tests'
			toComponentNamed: 'tests/Tests';
		yourself.
	packageName := aProjectName , '-Core'.
	className := aProjectName , 'Class1'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := aProjectName , '-Tests'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: aProjectName , 'TestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addPackageCore1DefinitionsFor_0004: projectDefinition [
	| className packageName |
	packageName := projectName , '-Core1'.
	className := projectName , 'Class1'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	^ projectDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addPlatformConditionalDefinitionsFor_0054Project: theProject condition: condition packageLabel: packageLabel [
	"Start with spec_0052; add platform conditional packages to the embedded project and the primary project."

	| theProjectName componentName1 componentName2 requiredClassName1 className1 className2 packageName1 gnuComponentName1 gnuComponentName2 gnuPackageName1 gnuClassName1 gnuPackageName2 |
	theProjectName := theProject name.

	componentName1 := 'Core'.
	componentName2 := 'Tests'.
	requiredClassName1 := theProjectName , 'Class1'.
	className1 := theProjectName , 'Class1'.
	className2 := theProjectName , 'TestCase'.
	packageName1 := theProjectName , '-Core'.

	gnuClassName1 := theProjectName , packageLabel, 'Class1'.
	gnuPackageName1 := theProjectName , '-', packageLabel, '-Core'.
	gnuPackageName2 := theProjectName , '-', packageLabel, '-Tests'.

	gnuComponentName1 := theProject
		addPlatformComponentNamed: componentName1
		toComponentNamed: componentName1
		pathNameArray: {packageLabel} 
		conditionPathArray: { {condition} }.
	gnuComponentName2 := theProject
		addPlatformComponentNamed: componentName2
		toComponentNamed: componentName1
		pathNameArray: {'tests' . packageLabel} 
		conditionPathArray: {'tests' . {condition}}.
	((theProject
		addPackageNamed: gnuPackageName1
		toComponentNamed: gnuComponentName1)
		addClassNamed: gnuClassName1
		super: className1
		instvars: #()
		category: gnuPackageName1
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^2' protocol: 'accessing';
		yourself.

	((theProject
		addPackageNamed: gnuPackageName2
		toComponentNamed: gnuComponentName2) addClassExtensionNamed: className2)
		addInstanceMethod:
				'test_' , gnuClassName1
						,
							'
	| x |
	self assert: (x := ' , gnuClassName1
						,
							' new foo) = 2.
'
			protocol: '*' , gnuPackageName2 asLowercase;
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addSimpleDefinitionsFor: resolvedProject projectName: aProjectName [
	| className packageName |
	resolvedProject
		addComponentNamed:  'tests/Tests'
			toComponentNamed: 'Core';
		addPackagesNamed: {(aProjectName , '-Core')}
			toComponentNamed: 'Core';
		addPackageNamed: aProjectName , '-Tests'
			toComponentNamed: 'tests/Tests';
		yourself.
	packageName := aProjectName , '-Core'.
	className := aProjectName , 'Class1'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := aProjectName , '-Tests'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: aProjectName , 'TestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _createCard: postfix specName: specName title: title index: index derivedFrom: parentbranch comment: comment [
	| indexName sha |
	indexName := 'index_' , postfix.
	sha := self preserveRowanSHA
		ifTrue: [ 
			self indexCardMap
				at: indexName
				ifAbsent: [ (RwProject newNamed: 'Rowan') repositoryCommitId ] ]
		ifFalse: [ (RwProject newNamed: 'Rowan') repositoryCommitId ].
	^ RwTestProjectLibraryIndexCard new
		name: indexName;
		title: title;
		specName: specName;
		index: index;
		derivedFrom: parentbranch;
		comment: comment;
		rowanIssues: {};
		gemstoneIssues: {};
		rowanSHA: sha;
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _createDefinedProject: aString [
	^ (Rowan newProjectNamed: aString)
		projectsHome: self projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
	| somethingToCommit |
	somethingToCommit := true.
	[ resolvedRepository commit: commitMessage ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex interaction message = 'Nothing to commit'
				ifFalse: [ ex pass ].
			somethingToCommit := false.
			ex response: nil ].
	(somethingToCommit and: [ self preserveChangesOnGithub ])
		ifTrue: [ 
			"only push if we really want to preserve the changes during this run and something has changed"
			resolvedRepository push: resolvedRepository remote branch: branchName ].
	^ somethingToCommit
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _finishCommitAndPush: specName indexCard: indexCard  resolvedRepository: resolvedRepository commitMessage: commitMessage [
	^ self
		_finishCommitAndPush: specName
		loadSpecification: nil
		indexCard: indexCard
		derivedFrom: nil
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _finishCommitAndPush: specName loadSpecification: theLoadSpec indexCard: indexCard derivedFrom: derivedFrom resolvedRepository: resolvedRepository commitMessage: commitMessage [
	| derivedIndexCardFile indexCardDict card |
	derivedFrom
		ifNil: [ 
			(resolvedRepository repositoryRoot / 'indexCards') exists
				ifTrue: [ (resolvedRepository repositoryRoot / 'indexCards') ensureDeleteAll ].
			(resolvedRepository repositoryRoot / 'specs') exists
				ifTrue: [ (resolvedRepository repositoryRoot / 'specs') ensureDeleteAll ] ]
		ifNotNil: [ 
			derivedIndexCardFile := resolvedRepository repositoryRoot / derivedFrom , 'ston'.
			derivedIndexCardFile exists
				ifTrue: [ derivedIndexCardFile delete ] ].
	indexCard exportTo: resolvedRepository repositoryRoot.

	theLoadSpec
		ifNotNil: [ 
			resolvedRepository repositoryRoot / 'README.md'
				writeStreamDo: [ :fileStream | 
					fileStream
						nextPutAll: '# ', projectName, ' - ' , specName;
						lf;
						nextPutAll: indexCard title;
						lf;
						nextPutAll: '```';
						lf;
						yourself.
					theLoadSpec exportToStream: fileStream.
					fileStream
						lf;
						lf.
					indexCard exportToStream: fileStream.
					fileStream
						lf;
						nextPutAll: '```';
						lf;
						lf;
						nextPutAll:
								'*This README file is autogenerated, so any direct edits may be lost.*';
						lf;
						yourself ] ].

	self
		_doCommitAndPush: commitMessage
		resolvedRepository: resolvedRepository
		branchName: specName.

	resolvedRepository checkout: 'master'.	"write the index card and spec on the master branch"
	resolvedRepository pull: resolvedRepository remote branch: 'master'.

	(resolvedRepository repositoryRoot / 'indexCards') ensureCreateDirectory.
	indexCard exportTo: resolvedRepository repositoryRoot / 'indexCards'.
	(resolvedRepository repositoryRoot / 'specs') ensureCreateDirectory.
	resolvedRepository _loadSpecification
		exportTo: resolvedRepository repositoryRoot / 'specs'.

	indexCardDict := Dictionary new.
	(resolvedRepository repositoryRoot / 'indexCards') allFiles
		do: [ :indexFile | 
			card := RwTestProjectLibraryIndexCard fromFile: indexFile.
			indexCardDict at: card specName put: card ].
	resolvedRepository repositoryRoot / 'README.md'
		writeStreamDo: [ :fileStream | 
			fileStream
				nextPutAll: '# ', projectName;
				lf;
				nextPutAll: self projectReadmeIntro;
				lf;
				nextPutAll: '## Summary of Load Specs';
				lf;
				yourself.
			(((resolvedRepository repositoryRoot / 'specs') allFiles
				collect: [ :specFile | RwSpecification fromFile: specFile ]) asArray
				sort: [ :a :b | a specName <= b specName ])
				do: [ :loadSpec | 
					fileStream
						nextPutAll: '### ' , loadSpec specName;
						lf;
						nextPutAll: '```';
						lf;
						yourself.
					loadSpec exportToStream: fileStream.
					(indexCardDict at: loadSpec specName ifAbsent: [  ])
						ifNotNil: [ :iCard | 
							fileStream
								lf;
								lf.
							iCard exportToStream: fileStream ].
					fileStream
						lf;
						nextPutAll: '```';
						lf;
						yourself ].
			fileStream
				lf;
				nextPutAll:
						'*This README file is autogenerated, so any direct edits may be lost.*';
				lf;
				yourself ].

	self
		_doCommitAndPush: commitMessage
		resolvedRepository: resolvedRepository
		branchName: 'master'
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _finishCommitAndPush: specName loadSpecification: loadSpecification indexCard: indexCard resolvedRepository: resolvedRepository commitMessage: commitMessage [
	^ self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
		indexCard: indexCard
		derivedFrom: nil
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor_004_b: specName branchName: branchName definedProject: definedProject addDefinitions: addDefinitionsBlock [
	"Create a second set of specs/components in an existing branch"

	| resolvedRepository resolvedProject |
	resolvedProject := definedProject resolveStrict.

	resolvedRepository := resolvedProject repository.
	(resolvedRepository branches includes: branchName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			resolvedRepository checkout: branchName.
			resolvedRepository pull: resolvedRepository remote branch: branchName ]
		ifFalse: [ self error: 'Expected branch ' , branchName printString , ' to already exist' ].

	resolvedProject revision: branchName.	"update the revision to match the current branch, since we want the revision set correctly when exported"
	addDefinitionsBlock value: resolvedProject defined.

	resolvedProject
		exportComponents;
		exportLoadSpecification.	"write the component and load spec directory structure"

	^ resolvedRepository
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor_080: specName definedProject: definedProject addDefinitions: addDefinitionsBlock [
	"Export the project using Pharo tonel format v1.0: Symbol keys and values"

	| resolvedRepository resolvedProject |
	resolvedProject := definedProject resolveStrict.

	resolvedRepository := resolvedProject repository.
	(resolvedRepository branches includes: specName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			resolvedRepository checkout: specName.
			resolvedRepository pull: resolvedRepository remote branch: specName ]
		ifFalse: [ 
			"create the branch"
			resolvedRepository createBranch: specName ].

	resolvedProject revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"

	resolvedProject projectRoots
		do: [ :rootDir | 
			"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
			rootDir ensureDeleteAll ].
	(resolvedProject repositoryRoot / 'rowan' / 'doits') ensureDeleteAll.

	addDefinitionsBlock value: resolvedProject defined.

	resolvedProject _resolvedProject
		exportPharoTonelFormatV1;
		exportLoadSpecification.	"write the rowan directory structure"

	^ resolvedRepository
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor: specName definedProject: definedProject addDefinitions: addDefinitionsBlock [
	"Coming in the load spec revision is pointing at the desired starting 
		point --- the structure to _start_ with. Since we don't know whether the 
		repository has been cloned or not, we do a resolve, which will ensure
		that the repository is on the revision specifiied in the incoming load spec.
	Once we've resolved and read, we reset the revision of the load spec
		to point to the branch we want to write to. We wipe out the project
		roots, so we have a clean sarting point and then export the project
		definition and load spec."

	| resolvedRepository resolvedProject |
	resolvedProject := definedProject resolveStrict.

	resolvedRepository := resolvedProject repository.
	(resolvedRepository branches includes: specName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			resolvedRepository checkout: specName.
			resolvedRepository pull: resolvedRepository remote branch: specName ]
		ifFalse: [ 
			"create the branch"
			resolvedRepository createBranch: specName ].

	resolvedProject revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"

	resolvedProject projectRoots
		do: [ :rootDir | 
			"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
			rootDir ensureDeleteAll ].
	(resolvedProject repositoryRoot / 'rowan' / 'doits') ensureDeleteAll.

	addDefinitionsBlock value: resolvedProject defined.

	resolvedProject
		export;
		exportLoadSpecification.	"write the rowan directory structure"

	^ resolvedRepository
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor: specName definedProject: definedProject embeddedProjectDefinition: embeddedProjectDefinition addDefinitions: addDefinitionsBlock [
	"Coming in the load spec revision is pointing at the desired starting 
		point --- the structure to _start_ with. Since we don't know whether the 
		repository has been cloned or not, we do a resolve, which will ensure
		that the repository is on the revision specifiied in the incoming load spec.
	Once we've resolved and read, we reset the revision of the load spec
		to point to the branch we want to write to. We wipe out the project
		roots, so we have a clean sarting point and then export the project
		definition and load spec."

	| resolvedRepository resolvedProject embeddedProject |
	resolvedProject := definedProject resolveStrict.
	embeddedProject := embeddedProjectDefinition resolveWithParentProject: resolvedProject.

	resolvedRepository := resolvedProject repository.
	(resolvedRepository branches includes: specName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			resolvedRepository checkout: specName.
			resolvedRepository pull: resolvedRepository remote branch: specName ]
		ifFalse: [ 
			"create the branch"
			resolvedRepository createBranch: specName ].

	resolvedProject revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"
	embeddedProject revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"

	resolvedProject projectRoots
		do: [ :rootDir | 
			"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
			rootDir ensureDeleteAll ].
	(resolvedProject repositoryRoot / 'rowan' / 'doits') ensureDeleteAll.

	embeddedProject projectRoots
		do: [ :rootDir | 
			"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
			rootDir ensureDeleteAll ].
	(embeddedProject repositoryRoot / 'rowan' / 'doits') ensureDeleteAll.

	addDefinitionsBlock value: resolvedProject defined value: embeddedProject defined.

	resolvedProject
		export;
		exportLoadSpecification.	"write the rowan directory structure"

	^ resolvedRepository
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _sampleSymbolDictionaryName1 [
	^ RwAbstractV2Test _sampleSymbolDictionaryName1
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _sampleSymbolDictionaryName2 [
	^ RwAbstractV2Test _sampleSymbolDictionaryName2
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _spec_0029_classCreationBlock [ 
^ [ :resolvedProject :theClassName :thePackageName :theTestClassName :theTestPackageName | 
	((resolvedProject packageNamed: thePackageName)
		addClassNamed: theClassName
		super: 'Object'
		instvars: #('ivar1')
		category: thePackageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	((resolvedProject packageNamed: theTestPackageName)
		addClassNamed: theTestClassName
		super: 'TestCase'
		category: theTestPackageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , theClassName , ' new foo = 1'
			protocol: 'tests';
		yourself ].
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _testRowanProjectsSandbox [
	^ RwAbstractV2Test _testRowanProjectsSandbox
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0000: commitMessage [
	"Create an initial project template with no packages -- basically an empty project.
		assume that github project already exists."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0000'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.
	title := 'Empty project with no packages'.

	indexCard := self
		_createCard: postfix
		specName: specName
		title: title
		index: 0
		derivedFrom: derivedFrom
		comment: ''.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0000: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0001: commitMessage [
	"Create a bare bones package structure -Core and -Tests, the tests should validate 
		the loaded state of project. Start with spec_0000 and add spec_0001 meat to the bones"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0001'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Bare bones package structure -Core and -Tests'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 1
		derivedFrom: derivedFrom
		comment:
			'One class per package: RowanSample9-Core and RowanSample9-Tests packages')
		rowanIssues: #(527);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0001: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0002: commitMessage [
	"RowanSample4 - basic project definition"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0002'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'RowanSample4 basic project definition'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 2
		derivedFrom: derivedFrom
		comment: 'RowanSampe9Class1 has extension methods')
		rowanIssues: #(254 504);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0002: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0003: commitMessage [
	"RowanSample4 - primer project definition"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0003'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'RowanSample4 primer project definition'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 3
		derivedFrom: derivedFrom
		comment: 'RowanSample9Class1 has instancesInvariant option set')
		rowanIssues: #(254);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0003: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0004_b: commitMessage [
	"https://github.com/GemTalk/Rowan/issues/532"

	"The method is NOT idempotent with respect to the branches involved. It must be run whenever genSpec_0004 is run.
		Right now it is called from genSpec_0004_b, so explicit top-level call is not necessary."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom branchName title |
	postfix := '0004_b'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.
	branchName := 'spec_0004'.
	title := 'Create inconsistency in component definitions ... duplicate package error expected on load'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 4
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(532);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core3'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor_004_b: specName
		branchName: branchName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0004_b: resolvedProject ].

	self
		_finishCommitAndPush: branchName
		indexCard: indexCard
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0004: commitMessage [
	"Reconcile inconsistencies for storage of package-specific properties. This set of definitions, uses
		three methods sent to a project definition to set the symbol dictionary for packages: as a
		starting point to see which data structures are written to disk."

	"
	projectDefinition
		addPackageNamed: projectName , '-Core1'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core'
			gemstoneDefaultSymbolDictionaryForUser: userId -> RwAbstractV2Test _sampleSymbolDictionaryName1;
		addPackageNamed: projectName , '-Core2'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		gemstoneSetSymbolDictName: RwAbstractV2Test _sampleSymbolDictionaryName2 forPackageNamed: projectName , '-Core2';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
		gemstoneSetDefaultSymbolDictNameForUser: userId to: RwAbstractV2Test _sampleSymbolDictionaryName3;
	"

	"https://github.com/GemTalk/Rowan/issues/531"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0004'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.
	title := 'Reconcile inconsistencies for storage of package-specific properties'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 4
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(500 531);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core1'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0004: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage.

	self genSpec_0004_b: commitMessage , '_b'
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0005: commitMessage [
	"Two classes, two  packages, plus tests"

	"https://github.com/dalehenrich/Rowan/issues/230"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0005'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Two classes, two  packages, plus tests. Primer for Issue 230'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 5
		derivedFrom: derivedFrom
		comment:
			'One class per package: RowanSample9-Core1, RowanSample9-Core2 and RowanSample9-Tests packages. Tests ensure that classes in Core1 are in '
				, RwRowanSample9Test _sampleSymbolDictionaryName2 printString
				, ' and classes in Core2 are in symbol dictionary '
				, RwRowanSample9Test _sampleSymbolDictionaryName3 printString
				, ' and classes in Tests are in symbol dictionary '
				, RwRowanSample9Test _sampleSymbolDictionaryName1 printString)
		rowanIssues: #(504);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0005: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0006: commitMessage [
	"spec_0002 with different symbol dict mappings for issue #504"

	"https://github.com/dalehenrich/Rowan/issues/504"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0006'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'spec_0002 with different symbol dict mappings'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 6
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core, RowanSample9-Extensions, RowanSample9-GemStone, RowanSample9-GemStone-Tests and RowanSample9-Tests packages. Tests ensure that all of the classes are in the proper symbol dictionary')
		rowanIssues: #(504);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0006: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0007: commitMessage [
	"Two classes, two  packages, plus tests"

	"https://github.com/dalehenrich/Rowan/issues/504"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0007'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0005'.
	title := 'Two classes, two  packages, plus tests. Primer for Issue 230, using ivs instead of instancesInvariant'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 7
		derivedFrom: derivedFrom
		comment:
			'One class per package: RowanSample9-Core1, RowanSample9-Core2 and RowanSample9-Tests packages. Tests ensure that classes in Core1 are in '
				, RwRowanSample9Test _sampleSymbolDictionaryName2 printString
				, ' and classes in Core2 are in symbol dictionary '
				, RwRowanSample9Test _sampleSymbolDictionaryName3 printString
				, ' and classes in Tests are in symbol dictionary '
				, RwRowanSample9Test _sampleSymbolDictionaryName1 printString)
		rowanIssues: #(504);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0007: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0008: commitMessage [
	"two symbol dicts ... gemstone in one, the rest in another"

	"https://github.com/dalehenrich/Rowan/issues/493"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0008'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0006'.
	title := 'two symbol dicts ... gemstone in one, the rest in another'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 8
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core, RowanSample9-Extensions, and RowanSample9-Tests packages in '
				, RwRowanSample9Test _sampleSymbolDictionaryName1 printString
				, '. RowanSample9-GemStone and RowanSample9-GemStone-Tests packages in '
				, RwRowanSample9Test _sampleSymbolDictionaryName2 printString)
		rowanIssues: #(493);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0008: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0009: commitMessage [
	"two symbol dicts ... gemstone in one symbol dict, the rest in the default symbol dict"

	"https://github.com/dalehenrich/Rowan/issues/493"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0009'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0008'.
	title := 'one symbol dict specified for gemstone, the rest in default (unspecified)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 9
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core, RowanSample9-Extensions, and RowanSample9-Tests packages in default. RowanSample9-GemStone and RowanSample9-GemStone-Tests packages in '
				, RwRowanSample9Test _sampleSymbolDictionaryName2 printString)
		rowanIssues: #(493);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0009: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0010: commitMessage [
	"two symbol dicts ... gemstone in one symbol dict, the rest in the default symbol dict --- default symbol changed from 0009"

	"https://github.com/dalehenrich/Rowan/issues/493"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0010'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0009'.
	title := 'one symbol dict specified for gemstone, the rest in default --- default sym dict changed from 0009'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 10
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core, RowanSample9-Extensions, and RowanSample9-Tests packages in default. RowanSample9-GemStone and RowanSample9-GemStone-Tests packages in '
				, RwRowanSample9Test _sampleSymbolDictionaryName2 printString)
		rowanIssues: #(493);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0010: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0011: commitMessage [
	"A single class with an extension method in a separate package. All packages loaded into a single symbol dict. Test methods validate classes and symbol dictionary mapping."

	"https://github.com/dalehenrich/Rowan/issues/495"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0011'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'A single class with an extension method in a separate package. All packages loaded into a single symbol dict. Test methods validate classes and symbol dictionary mapping.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 11
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core, RowanSample9-Extensions, and RowanSample9-Tests packages in default symbol dictionary.')
		rowanIssues: #(495);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0011: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0012: commitMessage [
	"Starting with spec_0011, move extension method to a new extension package; move class package to a 
		different symbol dict; move new extension package to a third symbol dictionary. This is an illegal combination
		(extension method and class in different symbol dictionaries) and does cause an error."

	"https://github.com/dalehenrich/Rowan/issues/495"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0012'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.
	title := 'Starting with spec_0011, move extension method to a new extension package; move class package to a 
		different symbol dict; move new extension package to a third symbol dictionary. This is an illegal combination
		(extension method and class in different symbol dictionaries) and does cause an error.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 12
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core in one symbol dict, RowanSample9-Extensions1 in another symbol dict, and 
		RowanSample9-Tests packages in default symbol dictionary.')
		rowanIssues: #(495);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0012: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0013: commitMessage [
	"Starting with spec_0011, move extension method to a new extension package; move class package to a 
		different symbol dict; move new extension package to the first symbol dictionary. This is an illegal combination
		(extension method and class in different symbol dictionaries) and does cause an error."

	"https://github.com/dalehenrich/Rowan/issues/495"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0013'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Starting with spec_0011, move extension method to a new extension package; move class package to a 
		different symbol dict; move new extension package to the first symbol dictionary. This is an illegal combination
		(extension method and class in different symbol dictionaries) and does cause an error.'
		index: 13
		derivedFrom: derivedFrom
		comment:
			'Swap symbol dicts for RowanSample9-Core and RowanSample9-Extensions1. RowanSample9-Tests
		packages in default symbol dictionary.')
		rowanIssues: #(495);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0013: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0014: commitMessage [
	"Starting with spec_0011, move extension method to a new extension package; move new extension package 
		to the classes symbol dictionary. This is a legal combination (extension method and class in same symbol
		dictionary) the V2 classes are handling the symbol dictionary lookup properly, so this combo validates that
		the bug (#493) is fixed."

	"https://github.com/dalehenrich/Rowan/issues/493"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0014'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Starting with spec_0011, move extension method to a new extension package; move new extension package 
		to the classes symbol dictionary. This is a legal combination (extension method and class in same symbol
		dictionary) the V2 classes are handling the symbol dictionary lookup properly, so this combo validates that
		the bug (#493) is fixed.'
		index: 14
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core and RowanSample9-Extensions1 in same symbol dictionary, but extension method moves
		to new package. RowanSample9-Tests package in default symbol dictionary.')
		rowanIssues: #(493);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0014: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0015: commitMessage [
	"Starting with spec_0014, remove the old (now empty) package from the project as triggering a package move
	as opposed to a class move seems to trigger the bug, which implies that move package is not quite right. 

	It seems that assigning a new symbol dictionary to the OLD package ... the package that the class is moved FROM
	may trigger a bogus package move.

	The test RwRowanProjectIssuesTestV2 >> testIssue495_move_class_and_extension_method_to_new_symbol_dict
	showed that #495 wasn't fixed by the V2 implementation."

	"https://github.com/dalehenrich/Rowan/issues/495"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0015'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0014'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Starting with spec_0014, remove the old (now empty) package from the project as triggering a package move
	as opposed to a class move seems to trigger the bug, which implies that move package is not quite right. 

	It seems that assigning a new symbol dictionary to the OLD package ... the package that the class is moved FROM
	may trigger a bogus package move.

	The test RwRowanProjectIssuesTestV2 >> testIssue495_move_class_and_extension_method_to_new_symbol_dict
	showed that #493 wasn''t fixed by the V2 implementation.'
		index: 15
		derivedFrom: derivedFrom
		comment:
			'RowanSample9-Core and RowanSample9-Extensions1 in same symbol dictionary, but extension method moves
		to RowanSample9-Extensions1. RowanSample9-Tests package in default symbol dictionary. Move the old (empty) package
		to a different symbol dictionary.')
		rowanIssues: #(495);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0015: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0016: commitMessage [
	"Starting with spec_0015, move an extension method from one package to another with
		no modifications"

	"https://github.com/GemTalk/Rowan/issues/557"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0016'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0015'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Starting with spec_0015, move an extension method from one package to another with no modifications'
		index: 16
		derivedFrom: derivedFrom
		comment:
			'Move extension method from  RowanSample9-Extensions1 to RowanSample9-Extensions.')
		rowanIssues: #(557);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0016: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0017: commitMessage [
	"Starting with spec_0015, change symbol dictionary of RowanSample9-Extensions to match symbol
		dictionary of RowanSample9-Extensions1"

	"https://github.com/GemTalk/Rowan/issues/557"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0017'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0015'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Starting with spec_0015, change symbol dictionary of RowanSample9-Extensions to match symbol dictionary of RowanSample9-Extensions'
		index: 17
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(557);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0017: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0018: commitMessage [
	"Build a sample vast/gemstone project based on information in https://github.com/dalehenrich/tonel-demos/tree/rowan_553"

	"https://github.com/GemTalk/Rowan/issues/553"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0018'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Build a sample vast/gemstone project based on information in https://github.com/dalehenrich/tonel-demos/tree/rowan_553'
		index: 18
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(553 555);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0018: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0019: commitMessage [
	"Conditional components test case"

	"https://github.com/GemTalk/Rowan/issues/554"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0019'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0017'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: 'Conditional components test case'
		index: 19
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(554);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0019: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0020: commitMessage [
	"Start with spec_0019, and undo work done to recreate spec_0017 structure -- find and fix holes in api"

	"https://github.com/GemTalk/Rowan/issues/554"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0020'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0019'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: 'Conditional components test case'
		index: 20
		derivedFrom: derivedFrom
		comment:
			'Start with spec_0019, and undo work done to recreate spec_0017 structure -- find and fix holes in api')
		rowanIssues: #(554);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0020: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0021: commitMessage [
	"Start with spec_0011, add pre and post load doits plus tests to validate that post load doits had been executed (part 1). Preload doit validation will be in part 2."

	"https://github.com/GemTalk/Rowan/issues/568"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0021'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with spec_0011, add pre and post load doits plus tests to validate that post load doits had been executed (part 1). Preload doit validation will be in part 2.'
		index: 21
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(568);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0021: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0022: commitMessage [
	"Start with spec_0021, add pre and post load doits adjust tests to validate that pre load doits had been executed (part 2)"

	"https://github.com/GemTalk/Rowan/issues/568"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0022'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0021'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with spec_0021, add pre and post load doits adjust tests to validate that pre load doits had been executed (part 2)'
		index: 22
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(568);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0022: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0023: commitMessage [
	"Start with  spec_0011, add a sequence of packages each subclassing off of the the other to be loaded in order."

	"https://github.com/GemTalk/Rowan/issues/568"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0023'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with spec_0011, add a sequence of packages each subclassing off of the the other to be loaded in order. The Core component is a sequenced component.'
		index: 23
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(568);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0023: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0024: commitMessage [
	"Start with  spec_0011, add pharo, gemstone, and vast conditional packages using
		RwSimpleProjectLoadComponentV2, intended to form the basis for working out the
		details necessary to enable pharo, gemstone and vast to share common code."

	"https://github.com/GemTalk/Rowan/issues/553"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0024'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0011, add pharo, gemstone, and vast conditional packages using
		RwSimpleProjectLoadComponentV2, intended to form the basis for working out the
		details necessary to enable pharo, gemstone and vast to share common code.'
		index: 24
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(553);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0024: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0025: commitMessage [
	"Start with  spec_0001, add 3 embedded projects"

	"https://github.com/GemTalk/Rowan/issues/571"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0025'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0001, add 3 embedded projects'
		index: 25
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(571);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0025: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0026: commitMessage [
	"Start with  spec_0000, create a project using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"start with a single component, single package project."

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0026'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0000, create a project using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 26
		derivedFrom: derivedFrom
		comment: 'start with a single component, single package project.')
		rowanIssues: #(573);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0026: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0027: commitMessage [
	"Start with  spec_0026, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"split pacakges into core and tests and use move class api to move class definitions from package to package"

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0027'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0026'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0026, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 27
		derivedFrom: derivedFrom
		comment:
			'split pacakges into core and tests and use move class api to move class definitions from package to package')
		rowanIssues: #(573);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {projectName};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0027: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0028: commitMessage [
	"Start with  spec_0027, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"add conditional Tests component"

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0028'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0027'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0027, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 28
		derivedFrom: derivedFrom
		comment: 'add conditional Tests component')
		rowanIssues: #(573);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {projectName};
		customConditionalAttributes: {'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0028: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0029: commitMessage [
	"Start with  spec_0028, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"add 2 more classes, tests and packages (one class/package) and create category components: Core, Definitions, Specs, Tests"

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0029'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0028'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0028, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 29
		derivedFrom: derivedFrom
		comment: 'add 2 more classes, tests and packages (one class/package) and create category components: Core, Definitions, Specs, Tests')
		rowanIssues: #(573);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {projectName};
		customConditionalAttributes: #('tests');
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0029: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0030: commitMessage [
	"Start with  spec_0029, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"add platform conditional extension methods and classes plus tests - gemstone pharo gs3.5-"

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0030'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0029'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0029, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 30
		derivedFrom: derivedFrom
		comment: 'add platform conditional extension methods and classes plus tests - gemstone pharo gs3.5-')
		rowanIssues: #(573);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {projectName};
		customConditionalAttributes: #('tests');
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0030: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0031: commitMessage [
	"Start with  spec_0029, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"Add platform conditional extension methods and classes plus tests - pharo, gemstone and gs3.[5-]. Use platform components with alias."

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0031'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0029'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0029, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 31
		derivedFrom: derivedFrom
		comment: 'Add platform conditional extension methods and classes plus tests - pharo, gemstone and gs3.[5-]. Use platform components with alias. Use leading _ character when using alias, to distiguish from attribute (and path) using the same name.')
		rowanIssues: #(573);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {projectName};
		customConditionalAttributes: #('tests');
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0031: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0032: commitMessage [
	"Start with  spec_0031, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project."

	"Explore the use of shared directory for sharing code between two conditions, like v1 and v2 ... first create v1 and v2 packages (that can be independently loaded or loaded together in GemStone and use the class in shared component for code that is common to both -- not controlled by attributes"

	"https://github.com/GemTalk/Rowan/issues/573"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0032'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0031'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0031, update the project structure using the new component creation API --- spec_0026 thru ?? represents the evolution of a project.'
		index: 32
		derivedFrom: derivedFrom
		comment: 'Explore the use of shared directory for sharing code between two conditions, like v1 and v2 ... first create v1 and v2 packages (that can be independently loaded or loaded together in GemStone and use the class in shared component for code that is common to both -- not controlled by attributes.')
		rowanIssues: #(573 571);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {projectName};
		customConditionalAttributes: #('tests');
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0032: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0033: commitMessage [
	"Start with  spec_0011, update class definition to match RowanSample1 branch issue_345, i.e., add constraints to a class - porting RwRowanSample1Test>>testIssue345 and RwRowanSample1Test>>testIssue514 to v2"

	"https://github.com/GemTalk/Rowan/issues/345; https://github.com/GemTalk/Rowan/issues/514"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom |
	postfix := '0033'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Start with  spec_0011, update class definition to match RowanSample1 branch issue_345, i.e., add constraints to a class - porting RwRowanSample1Test>>testIssue345 and RwRowanSample1Test>>testIssue514 to v2'
		index: 33
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(345 514);
		yourself.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: #('tests');
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		specComment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0033: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0034: commitMessage [
	"Start with spec_0001, remove the instance variable. Foundation for instance migration testing - _migration_0."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0034'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001, remove the instance variable. Foundation for instance migration testing - _migration_0.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 34
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  {};
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0034: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0035: commitMessage [
	"Start with spec_0034, add class hierarchy with instance variables and methods. _migration_1."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0035'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0034'.
	title := 'Start with spec_0034, add class hierarchy with instance variables and methods. _migration_1.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 35
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0035: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0036: commitMessage [
	"Start with spec_0035, move ivs ivar1 and ivar2 up to the top of the hierarcy; methods should be okay. _migration_2."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0036'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0035'.
	title := 'Start with spec_0035, move ivs ivar1 and ivar2 up to the top of the hierarcy; methods should be okay. _migration_2.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 36
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0036: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0037: commitMessage [
	"Start with spec_0002, add constraints to RowanSample9Class1 and RowanSample9IdentityKeyValueDictionary, plus tests to validate the constraints"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0037'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'Start with spec_0002, add constraints to RowanSample9Class1 and RowanSample9IdentityKeyValueDictionary, plus tests to validate the constraints'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 37
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(14);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0037: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0038: commitMessage [
	"Start with spec_0002, add new class in new package. spec_0002 = issue_185_0, spec_0038 = issue_185_1"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0038'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'Start with spec_0002, add new class in new package. spec_0002 = issue_185_0, spec_0038 = issue_185_1'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 38
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0038: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0039: commitMessage [
	"Start with spec_0038, add class var to NewRowanSample9Class (issue_185_7)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0039'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0038'.
	title := 'Start with spec_0038, add class var to NewRowanSample9Class (issue_185_7)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 39
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0039: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0040: commitMessage [
	"Start with spec_0038, add inst var to NewRowanSample9Class (create new class version) (issue_185_5)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0040'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0038'.
	title := 'Start with spec_0038, add inst var to NewRowanSample9Class (create new class version)(issue_185_5)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 40
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0040: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0041: commitMessage [
	"Start with spec_0038, rename package to RowanSample9-RenamedPackage (issue_185_2)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0041'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0038'.
	title := 'Start with spec_0038, rename package to RowanSample9-RenamedPackage (issue_185_2)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 41
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0041: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0042: commitMessage [
	"Start with spec_0039, rename package to RowanSample9-RenamedPackage (issue_185_8)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0042'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0039'.
	title := 'Start with spec_0039, rename package to RowanSample9-RenamedPackage (issue_185_8)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 42
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0042: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0043: commitMessage [
	"Start with spec_0040, rename package to RowanSample9-RenamedPackage (issue_185_6)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0043'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0040'.
	title := 'Start with spec_0040, rename package to RowanSample9-RenamedPackage (issue_185_6)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 43
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0043: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0044: commitMessage [
	"Start with spec_0038, rename package to RowanSample9-RenamedPackage (issue_185_4)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0044'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0038'.
	title := 'Start with spec_0038, rename package to RowanSample9-RenamedPackage (issue_185_4)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 44
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(185);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0044: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0045: commitMessage [
	"Start with spec_0038, change a comment in the component (benign commit)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0045'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0038'.
	title := 'Start with spec_0038, change a comment in the component (benign commit)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 45
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(284);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0045: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0046: commitMessage [
	"Start with spec_0041, add inst vars to NewRowanSample9Class and rename the rest of the packages (add _295) to package name (issue_295_2)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0046'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0041'.
	title := 'Start with spec_0041, add inst vars to NewRowanSample9Class and rename the rest of the packages (add _295) to package name (issue_295_2)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 46
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(295);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0046: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0047: commitMessage [
	"Start with spec_0040, move RowanSample9IdentityKeyValueDictionary to -Core package; add subclass of NewRowanSample9Class in New package  (issue_295_4)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0047'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0040'.
	title := 'Start with spec_0040, move RowanSample9IdentityKeyValueDictionary to -Core package; add subclass of NewRowanSample9Class in New package (issue_295_4)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 47
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(295);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0047: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0048: commitMessage [
	"Start with spec_0046; add subclass of NewRowanSample9Class in Renamed package (issue_295_5); for issue #304, make Renamed package conditional of renamed attribute"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0048'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0046'.
	title := 'Start with spec_0046; add subclass of NewRowanSample9Class in Renamed package (issue_295_5); for issue #304, make Renamed package conditional on renamed attribute'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 48
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(295 304);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' . 'renamed'};
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0048: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0049: commitMessage [
	"Start with spec_0048; create an additional load spec without the renamed atribute"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0049'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0048'.
	title := 'Start with spec_0048; create an additional load spec without the renamed atribute'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 49
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(304);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' . 'renamed'};
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0049: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0050: commitMessage [
	"Start with spec_0047; move RowanSample9IdentityKeyValueDictionary to RowanSample9-GemStone (issue_295_6)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0050'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0047'.
	title := 'Start with spec_0047; move RowanSample9IdentityKeyValueDictionary to RowanSample9-GemStone (issue_295_6)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 50
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(460);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' . 'renamed'};
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0050: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0051: commitMessage [
	"Start with spec_0002; add a component that includes tests as part of core attributes (issue_493)"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0051'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'Start with spec_0002; add a component that includes tests as part of core attributes (issue_493)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 51
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(493);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0051: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0052: commitMessage [
	"Start with spec_0001; add an embedded project that is basically a copy of spec_001; and adjust class in primary project be a subclass of class in embedded project"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0052'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001; add an embedded project that is basically a copy of spec_001; and adjust class in primary project be a subclass of class in embedded project'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 52
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(591 571);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0052: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0053: commitMessage [
	"Start with spec_0052; add instance side method #ivar1 referencing inherited ivar to class in primary project"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0053'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0052'.
	title := 'Start with spec_0052; add instance side method #ivar1 referencing inherited ivar to class in primary project'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 53
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(591);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0053: resolvedProject ].	"WARNING should be using loadSpecification:requiredLoadSpecification:addDefinitions:; follow the genSpec_0054 model"

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0054: commitMessage [
	"Start with spec_0052; add platform conditional packages to the embedded project and the primary project."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embeddedProjectDefinition requiredProjectName embeddedProjectReference embededProjectDirName |
	postfix := '0054'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0052'.
	title := 'Start with spec_0052; add platform conditional packages to the embedded project and the primary project.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 54
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(594);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		specComment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	embededProjectDirName := 'embeddedProjects' .
	embeddedProjectReference := Path * embededProjectDirName / requiredProjectName
		/ 'rowan'.
	embeddedProjectDefinition :=  (Rowan newEmbeddedProjectNamed: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: (embeddedProjectReference / 'project' , 'ston') pathString;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embeddedProjectDefinition
		addDefinitions: [ :resolvedProject :embeddedProject | self _addDefinitionsFor_0054: resolvedProject embeddedProject: embeddedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0055: commitMessage [
	"Start with spec_0052; add platform conditional packages (GemStone version 2.4.0) to the embedded project and the primary project."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embededProjectDefinition requiredProjectName embeddedProjectReference embededProjectDirName |
	postfix := '0055'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0052'.
	title := 'Start with spec_0052; add platform conditional packages (GemStone version 2.4.0) to the embedded project and the primary project.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 55
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(594);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		specComment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	embededProjectDirName := 'embeddedProjects' .
	embeddedProjectReference := Path * embededProjectDirName / requiredProjectName
		/ 'rowan'.
	embededProjectDefinition :=  (Rowan newEmbeddedProjectNamed: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: (embeddedProjectReference / 'project' , 'ston') pathString;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embededProjectDefinition
		addDefinitions: [ :resolvedProject :embeddedProject | self _addDefinitionsFor_0055: resolvedProject embeddedProject: embeddedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0056: commitMessage [
	"Start with spec_0052; add platform conditional packages (GemStone version 3.6.x, 3.5.x, and 3.2.15) to the embedded project and the primary project. This is expected to reproduce issue #594 for those three patforms."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title embededProjectDefinition requiredProjectName embeddedProjectReference embededProjectDirName |
	postfix := '0056'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0052'.
	title := 'Start with spec_0052; add platform conditional packages (GemStone version 3.6.x, 3.5.x, and 3.2.15) to the embedded project and the primary project. This is expected to reproduce issue #594 for those three patforms.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 56
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(594);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		specComment: title;
		yourself.

	requiredProjectName := projectName , '_embedded_1'.
	embededProjectDirName := 'embeddedProjects' .
	embeddedProjectReference := Path * embededProjectDirName / requiredProjectName
		/ 'rowan'.
	embededProjectDefinition :=  (Rowan newEmbeddedProjectNamed: requiredProjectName)
		specName: requiredProjectName;
		projectAlias: projectName;
		projectSpecFile: (embeddedProjectReference / 'project' , 'ston') pathString;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		customConditionalAttributes: { 'tests' }
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		embeddedProjectDefinition: embededProjectDefinition
		addDefinitions: [ :resolvedProject :embededProject | self _addDefinitionsFor_0056: resolvedProject embeddedProject: embededProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0057: commitMessage [
	"Start with spec_0000, use Rowan packageConvention; simple project with extension methods"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0057'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Start with spec_0000, use Rowan packageConvention; simple project with extension methods'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 3
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		packageConvention: 'Rowan';
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0057: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0058: commitMessage [
	"Start with spec_0057 and set class category to nil and empty string (https://github.com/GemTalk/Rowan/issues/169)"

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0058'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0057'.
	title := 'Start with spec_0057 and set class category to nil and empty string (https://github.com/GemTalk/Rowan/issues/169)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 3
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		packageConvention: 'Rowan';
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0058: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0059: commitMessage [
	"Component api revamp. Start with spec_0001; add 3 conditions: alt1, alt2, alt3; one extension method and test case per condition (see https://github.com/GemTalk/Rowan/issues/653 and https://github.com/GemTalk/Rowan/issues/660)"

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0059'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Component api revamp. Start with spec_0001; add 3 conditions: alt1, alt2, alt3; one extension method and test case per condition (see https://github.com/GemTalk/Rowan/issues/653 and https://github.com/GemTalk/Rowan/issues/660)'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 3
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #( 653 660 );
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0059: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0060: commitMessage [
	"Start with spec_0001; add RowanSample10 as an external project ... based on genSpec_0052"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0060'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001; add RowanSample10 as an external project ... based on genSpec_0052'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 60
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(668);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0060: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0064: commitMessage [
	"Start with spec_0060; WITHOUT RowanSample10 as an external project ... and add some packages that duplicate RowanSample10 package names and content ... in concert with a reload of spec_0060 reproduce https://github.com/GemTalk/Rowan/issues/680"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0064'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0060'.
	title := 'Start with spec_0060; WITHOUT RowanSample10 as an external project ... and add some packages that duplicate RowanSample10 package names and content ... in concert with a reload of spec_0060 reproduce https://github.com/GemTalk/Rowan/issues/680'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 61
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(680);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0064: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0079: commitMessage [
	"Start with spec_0002; and write in filetree format instead of tonel format."

	"https://github.com/GemTalk/Rowan/issues/761"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0079'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'Start with spec_0002; and write in filetree format instead of tonel format.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 79
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(761);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0079: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0080: commitMessage [
	"Start with spec_0002; read and write in Pharo tonel format v1.0 (keys and values are Symbols)."

	"https://github.com/GemTalk/Rowan/issues/777"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0080'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.
	title := 'Start with spec_0002; read and write in Pharo tonel format v1.0 (keys and values are Symbols).'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 80
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(777);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes:  { 'tests' };
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor_080: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0080: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification:  resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0081: commitMessage [
	"Start with spec_0011; add 2 more instance methods and 3 class methods to RowanSample9Class1 in RowanSample9-Core; then add 3 instance-side extension methods to RowanSample9Class1 and 2 more class-side extension methods; add tests for all of the new methods. Fodder for audit testing."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0081'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0011'.
	title := 'Start with spec_0011; add 2 more instance methods and 3 class methods to RowanSample9Class1 in RowanSample9-Core; then add 3 instance-side extension methods to RowanSample9Class1 and 2 more class-side extension methods; add tests for all of the new methods. Fodder for audit testing.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 81
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		specComment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0081: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> indexCardMap [
	^ indexCardMap
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> preserveChangesOnGithub [
	"if true, modifications to git repository will be pushed to remote"

	^ preserveChangesOnGithub ifNil: [ preserveChangesOnGithub := true ]
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> preserveChangesOnGithub: aBool [
	"if true, modifications to git repository will be pushed to remote"

	preserveChangesOnGithub := aBool
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> preserveRowanSHA [
	^ preserveRowanSHA ifNil: [ false ]
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> preserveRowanSHA: aBool [
	preserveRowanSHA := aBool
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> primeIndexCardMap [
	self preserveRowanSHA
		ifFalse: [ ^ self ].
	RwAbstractV2Test _testRowanProjectsSandbox ensureCreateDirectory.
	Rowan projectTools git
		gitcloneIn: RwAbstractV2Test _testRowanProjectsSandbox pathString
		with: 'https://git@github.com:dalehenrich/' , projectName.
	indexCardMap := Dictionary new.
	(RwAbstractV2Test _testRowanProjectsSandbox / projectName / 'indexCards') files
		do: [ :file | 
			| indexCard |
			file
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					indexCard := STON fromStream: stream ].
			indexCardMap at: indexCard name put: indexCard rowanSHA ]
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectName [
	^projectName
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectName: object [
	projectName := object
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectReadmeIntro [
	"Intro in README"

	^ 'Sample project that is being used to develop the **final** version of the V2.0 project handling API.'
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectsHome [
	^ projectsHome ifNil: [ self _testRowanProjectsSandbox ]
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectsHome: aString [
	projectsHome := aString asFileReference
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectUrl [
	^projectUrl
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectUrl: object [
	projectUrl := object
]
