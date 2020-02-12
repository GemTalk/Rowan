"
| commitComment |
RwRowanSample9Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

commitComment := ''.
RwTestProjectLibraryGenerator new
	projectName: 'RowanSample9';
	projectUrl: 'https://github.com/dalehenrich/RowanSample9';
	preserveChangesOnGithub: true;
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
	yourself
"
Class {
	#name : 'RwTestProjectLibraryGenerator',
	#superclass : 'Object',
	#instVars : [
		'projectsHome',
		'projectName',
		'projectUrl',
		'preserveChangesOnGithub'
	],
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addApplication_SubApplicationComponentsFor_0023: resolvedProject [
	"Start with  spec_0011, add pharo, gemstone, and vast conditional packages using
		RwSimpleProjectLoadComponentV2, intended to form the basis for working out the
		details necessary to enable pharo, gemstone and vast to share common code."

	"https://github.com/GemTalk/Rowan/issues/553"

	| packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 className1 className2 |
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

	resolvedProject
		addApplicationNamed: 'envy/Core'
			condition: 'common'
			comment:
				'Top-level application for loading the application core. NOTE: boty envy/Core and envy/Tests are recursive';
		addApplicationNamed: 'envy/Tests'
			condition: 'common'
			comment: 'Top-level application for the application tests';
		addSubApplicationNamed: 'envy/platform/Extensions-GemStone'
			condition: 'gemstone'
			comment: 'Subapplication for loading gemstone-specific packages';
		addSubApplicationNamed: 'envy/platform/Extensions-Pharo'
			condition: 'pharo'
			comment: 'Subapplication for loading pharo-specific packages';
		addApplicationNamed: 'envy/Extensions-VAST'
			condition: 'vast'
			comment:
				'Subapplication for loading vast-specific test packages and components. 
		The component named STON is a pseudo-component that refers to 
		applications expected to reside in the VAST image.

		NOTE: I want this application to be be a subapplication --- it is not a COMPLETE application.
		It MUST be created in order to reference the VAST specific STON application.';
		addApplicationNamed: 'envy/Tests-VAST'
			condition: 'vast'
			comment:
				'Subapplication for loading vast-specific test packages and components. 
		The component named SUnit is a pseudo-component that refers to 
		applications expected to reside in the VAST image.

		NOTE: I want this application to be be a subapplication --- it is not a COMPLETE application.
		It MUST be created in order to reference the VAST specific STON application.';
		yourself.

	resolvedProject
		addApplicationNamed: 'envy/Tests' toComponentNamed: 'envy/Core';
		addApplicationNamed: 'envy/Core' toComponentNamed: 'envy/Tests';
		addApplicationNamed: 'envy/Tests-VAST' toComponentNamed: 'envy/Tests';
		addApplicationNamed: 'envy/platform/Extensions-GemStone'
			toComponentNamed: 'envy/Core';
		addApplicationNamed: 'envy/platform/Extensions-Pharo'
			toComponentNamed: 'envy/Core';
		addApplicationNamed: 'envy/Extensions-VAST' toComponentNamed: 'envy/Core';
		yourself.

	resolvedProject
		addPackagesNamed:
				{packageName1.
					packageName2}
			toComponentNamed: 'envy/Core';
		addPackageNamed: packageName3 toComponentNamed: 'envy/Tests';
		addPackageNamed: packageName4
			toComponentNamed: 'envy/platform/Extensions-GemStone';
		addPackageNamed: packageName5
			toComponentNamed: 'envy/platform/Extensions-Pharo';
		addPackagesNamed:
				{packageName6.
					packageName7}
			toComponentNamed: 'envy/Extensions-VAST';
		addPackageNamed: packageName8 toComponentNamed: 'envy/Tests-VAST';
		yourself.

	resolvedProject
		addApplicationNamed: 'STON' toComponentNamed: 'envy/Extensions-VAST';
		addApplicationNamed: 'Sunit' toComponentNamed: 'envy/Tests-VAST';
		yourself.	"add the VAST image application dependencies"

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0000: resolvedProject [
	resolvedProject
		comment: 'spec_0000 definitions';
		addComponentNamed: 'Core'
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						add: 'tests' -> {'core'};
						yourself)
			comment: 'Primary component used for loading the core classes.';
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
		addPackagesNamed:
				{packageName1.
				packageName2.
				packageName3}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addPackagesNamed:
				{packageName4.
				packageName5}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
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
	| userId |
	userId := 'SystemUser'.
	(resolvedProject
		addComponentNamed: 'Core3'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment:
			'contains ' , projectName
				,
					'-Core1 package, which is a duplicate reference, since the Core1 component also references the '
				, projectName , '-Core1 package')
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: #('Core1');
		yourself.
	resolvedProject
		addPackageNamed: projectName , '-Core1'
			toComponentNamed: 'Core3'
			withConditions: {'common'}
			andGroupName: 'core';
		yourself.
	^ self _addPackageCore1DefinitionsFor_0004: resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0004: resolvedProject [
	| className packageName userId |
	userId := resolvedProject _gemstoneAllUsersName.
	(resolvedProject
		addComponentNamed: 'Core1'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core1 package')
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: #('Core2');
		yourself.
	(resolvedProject
		addComponentNamed: 'Core2'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core2 package')
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: #('Core1');
		yourself.
	resolvedProject
		addPackageNamed: projectName , '-Core1'
			toComponentNamed: 'Core1'
			withConditions: {'common'}
			andGroupName: 'core'
			gemstoneDefaultSymbolDictionaryForUser:
				userId -> RwAbstractV2Test _sampleSymbolDictionaryName2;
		addPackageNamed: projectName , '-Core2'
			toComponentNamed: 'Core2'
			withConditions: {'common'}
			andGroupName: 'core';
		gemstoneSetSymbolDictName: RwAbstractV2Test _sampleSymbolDictionaryName3
			forPackageNamed: projectName , '-Core2';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'Core1'
			withConditions: {'common'}
			andGroupName: 'tests';
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
		addPackagesNamed:
				{packageName1.
				packageName2}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addPackagesNamed:
				{packageName3}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
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
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
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
			withConditions: {'common'}
			andGroupName: 'core';
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
			withConditions: {'common'}
			andGroupName: 'core';
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
			withConditions: {'common'}
			andGroupName: 'core';
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

	| componentName groupName packageName1 packageName2 extensionClassName1 className1 className2 |
	componentName := 'Core'.
	groupName := 'core'.
	packageName1 := projectName , '-Vast-Application'.
	packageName2 := projectName , '-Common-Core'.
	extensionClassName1 := 'TestCase'.
	className1 := 'TonelExampleTest'.
	className2 := 'TonelExampleApp'.

	resolvedProject projectSpecification
		packagesPath: 'source';
		packageConvention: 'Rowan';
		yourself.

	resolvedProject loadSpecification
		groupNames: {groupName};
		yourself.

	resolvedProject
		comment:
				'sample vast/gemstone project, based on project structure in https://github.com/dalehenrich/tonel-demos/tree/rowan_553';
		addComponentNamed: componentName
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						yourself)
			comment: 'Primary component used for loading the core classes.';
		addPackageNamed: packageName1
			toComponentNamed: componentName
			withConditions: {'vast'}
			andGroupName: groupName;
		addPackageNamed: packageName2
			toComponentNamed: componentName
			withConditions: {'common'}
			andGroupName: groupName;
		yourself.

	(resolvedProject packageNamed: packageName1)
		propertyAt: 'comment'
			put:
				'This is a vast only package, so the vast package properties are required?';
		propertyAt: 'vaPrerequisites' put: #('SUnit' 'TonelWriterModel');
		propertyAt: 'vaSubApplications'
			put:
				(Dictionary new
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
		addNestedComponentNamed: componentName1
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						add: 'tests' -> {'core'};
						yourself)
			comment: '';
		addNestedComponentNamed: componentName2
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						add: 'tests' -> {'core'};
						yourself)
			comment: '';
		gemstoneSetSymbolDictName: nil forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: nil forPackageNamed: packageName2;
		removePackageNamed: packageName1
			fromComponentNamed: componentName
			withConditions: conditionArray
			andGroupName: groupName;
		removePackageNamed: packageName2
			fromComponentNamed: componentName
			withConditions: conditionArray
			andGroupName: groupName;
		addPackageNamed: packageName1
			toComponentNamed: componentName1
			withConditions: conditionArray
			andGroupName: groupName;
		addPackageNamed: packageName2
			toComponentNamed: componentName2
			withConditions: conditionArray
			andGroupName: groupName;

		addComponentNamed: componentName1
			toComponentNamed: componentName
			withConditions: conditionArray
			andGroupName: groupName;
		addComponentNamed: componentName2
			toComponentNamed: componentName
			withConditions: conditionArray
			andGroupName: groupName;

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
			toComponentNamed: componentName
			withConditions: conditionArray
			andGroupName: groupName;
		addPackageNamed: packageName2
			toComponentNamed: componentName
			withConditions: conditionArray
			andGroupName: groupName;
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
	resolvedProject components
		addSimpleComponentNamed: 'Core'
		condition: 'common'
		groupName: 'core'
		comment:
			'master component, that will load 4 simple components in order with each component doing a complete, independent load'.

	resolvedProject
		addSimpleComponentNamed: 'Core1'
			condition: 'common'
			groupName: 'core'
			comment: 'First component for loading first package';
		addPackagesNamed:
				{packageName1.
					testPackageName1}
			toComponentNamed: 'Core1';
		addSimpleComponentNamed: 'Core2'
			condition: 'common'
			groupName: 'core'
			comment: 'Second component for loading second package';
		addPackagesNamed:
				{packageName2.
					testPackageName2}
			toComponentNamed: 'Core2';
		addSimpleComponentNamed: 'Core3'
			condition: 'common'
			groupName: 'core'
			comment: 'Third component for loading third package';
		addPackagesNamed:
				{packageName3.
					testPackageName3}
			toComponentNamed: 'Core3';
		addSimpleComponentNamed: 'Core4'
			condition: 'common'
			groupName: 'core'
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
	filePath := (FileReference newTempFilePrefix: ''', projectName , '_export'' suffix: ''ston'') pathString.
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
		addSimpleComponentNamed: 'Core'
			condition: 'common'
			groupName: 'core'
			comment: 'Top-level component for loading the application';
		addSimpleComponentNamed: 'Tests'
			condition: 'common'
			groupName: 'tests'
			comment: 'Top-level component for the application tests';
		addSimpleNestedComponentNamed: 'platform/Extensions-GemStone'
			condition: 'gemstone'
			groupName: 'core'
			comment: 'Component for loading gemstone-specific packages';
		addSimpleNestedComponentNamed: 'platform/Extensions-Pharo'
			condition: 'pharo'
			groupName: 'core'
			comment: 'Component for loading pharo-specific packages';
		addSimpleNestedComponentNamed: 'platform/Extensions-VAST'
			condition: 'vast'
			groupName: 'core'
			comment:
				'Component for loading vast-specific test packages and components. 
		The component named VAST_IMAGE/STON is a pseudo-component that refers to 
		applications expected to reside in the VAST image.

		The package named RowanSample9-VAST-Application, which contains the Envy 
		Application class is also included in this component.';
		addSimpleNestedComponentNamed: 'platform/Tests-VAST'
			condition: 'vast'
			groupName: 'tests'
			comment:
				'Component for loading vast-specific test packages and components. 
		The component named VAST_IMAGE/SUnit is a pseudo-component that refers to 
		applications expected to reside in the VAST image';
		yourself.

	resolvedProject
		addComponentNamed: 'Tests' toComponentNamed: 'Core';
		addComponentNamed: 'Core' toComponentNamed: 'Tests';
		addComponentNamed: 'platform/Tests-VAST' toComponentNamed: 'Tests';
		addComponentNamed: 'platform/Extensions-GemStone' toComponentNamed: 'Core';
		addComponentNamed: 'platform/Extensions-Pharo' toComponentNamed: 'Core';
		addComponentNamed: 'platform/Extensions-VAST' toComponentNamed: 'Core';
		yourself.

	resolvedProject
		addPackagesNamed:
				{packageName1.
					packageName2}
			toComponentNamed: 'Core';
		addPackageNamed: packageName3 toComponentNamed: 'Tests';
		addPackageNamed: packageName4
			toComponentNamed: 'platform/Extensions-GemStone';
		addPackageNamed: packageName5 toComponentNamed: 'platform/Extensions-Pharo';
		addPackagesNamed:
				{packageName6.
					packageName7}
			toComponentNamed: 'platform/Extensions-VAST';
		addPackageNamed: packageName8 toComponentNamed: 'platform/Tests-VAST';
		yourself.

	resolvedProject
		addComponentNamed: 'VAST_IMAGE/STON'
			toComponentNamed: 'platform/Extensions-VAST';
		addComponentNamed: 'VAST_IMAGE/Sunit' toComponentNamed: 'platform/Tests-VAST';
		yourself.	"add the VAST_IMAGE application dependencies"

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

	self _addApplication_SubApplicationComponentsFor_0023: resolvedProject.

	^ resolvedProject
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0025: resolvedProject_0025 [
	"Start with  spec_0001, add 3 embedded projects"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpecification_0025 packageNames |
	loadSpecification_0025 := resolvedProject_0025 _loadSpecification.
	packageNames := resolvedProject_0025 packageNames.

	loadSpecification_0025 groupNames: {}.

	resolvedProject_0025
		removeComponentNamed: 'Core';
		addSimpleComponentNamed: 'Core' condition: 'common' comment: 'the component';
		yourself.
	packageNames
		do: [ :packageName | resolvedProject_0025 addPackageNamed: packageName toComponentNamed: 'Core' ].

	1 to: 3 do: [ :index | 
		| resolvedProjectName suffix loadSpecification specName resolvedProject baseName |
		suffix := '_' , index asString.
		specName := loadSpecification_0025 specName , suffix.
		resolvedProjectName := resolvedProject_0025 projectName , suffix.

		loadSpecification := RwEmbeddedLoadSpecificationV2 new
			projectName: resolvedProjectName;
			projectAlias: resolvedProject_0025 projectName;
			projectsHome: self projectsHome;
			gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
			groupNames: {};
			yourself.

		baseName := 'rowan' , suffix.
		loadSpecification
			specName: resolvedProjectName;
			revision: loadSpecification_0025 revision;
			gitUrl: self projectUrl;
			componentNames: {'Core'};
			comment: loadSpecification_0025 comment;
			yourself.

		resolvedProject := loadSpecification resolve.

		resolvedProject
			addSimpleComponentNamed: 'Core'
			condition: 'common'
			comment: 'the component'.

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

	resolvedProject loadSpecification
		componentNames: {componentName1};
		groupNames: #();
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
RwTestProjectLibraryGenerator >> _addDefinitionsFor: resolvedProject projectName: aProjectName [
	| className packageName |
	resolvedProject
		addPackagesNamed: {(aProjectName , '-Core')}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addPackageNamed: aProjectName , '-Tests'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
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
RwTestProjectLibraryGenerator >> _addSimpleDefinitionsFor: resolvedProject projectName: aProjectName [
	| className packageName |
	resolvedProject
		addPackagesNamed: {(aProjectName , '-Core')}
			toComponentNamed: 'Core';
		addPackageNamed: aProjectName , '-Tests'
			toComponentNamed: 'Core';
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
	^ RwTestProjectLibraryIndexCard new
		name: 'index_' , postfix;
		title: title;
		specName: specName;
		index: index;
		derivedFrom: parentbranch;
		comment: comment;
		rowanIssues: {};
		gemstoneIssues: {};
		rowanSHA: (RwProject newNamed: 'Rowan') repositoryCommitId;
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _createLoadSpecification: aString [
	^ RwLoadSpecificationV2 new
		projectName: aString;
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
			resolvedRepository push: resolvedRepository remote branch: branchName ]
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
						nextPutAll: '# RowanSample9 - ' , specName;
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
				nextPutAll: '# RowanSample9';
				lf;
				nextPutAll:
						'Sample project that is being used to develop the **final** version of the V2.0 project handling API.';
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
RwTestProjectLibraryGenerator >> _genSpecFor_004_b: specName branchName: branchName loadSpecification: loadSpecification addDefinitions: addDefinitionsBlock [
	"Create a second set of specs/components in an existing branch"

	| resolvedRepository resolvedProject |
	resolvedProject := loadSpecification resolveStrict.

	resolvedRepository := resolvedProject repository.
	(resolvedRepository branches includes: branchName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			resolvedRepository checkout: branchName.
			resolvedRepository pull: resolvedRepository remote branch: branchName ]
		ifFalse: [ self error: 'Expected branch ' , branchName printString , ' to already exist' ].

	resolvedProject revision: branchName.	"update the revision to match the current branch, since we want the revision set correctly when exported"
	addDefinitionsBlock value: resolvedProject.

	resolvedProject
		exportComponents;
		exportLoadSpecification.	"write the component and load spec directory structure"

	^ resolvedRepository
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor: specName loadSpecification: loadSpecification addDefinitions: addDefinitionsBlock [
	"Coming in the load spec revision is pointing at the desired starting 
		point --- the structure to _start_ with. Since we don't know whether the 
		repository has been cloned or not, we do a resolve, which will ensure
		that the repository is on the revision specifiied in the incoming load spec.
	Once we've resolved and read, we reset the revision of the load spec
		to point to the branch we want to write to. We wipe out the project
		roots, so we have a clean sarting point and then export the project
		definition and load spec."

	| resolvedRepository resolvedProject |
	resolvedProject := loadSpecification resolveStrict.

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
	(resolvedProject repositoryRoot / 'rowan/doits') ensureDeleteAll.

	addDefinitionsBlock value: resolvedProject.

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
RwTestProjectLibraryGenerator >> _testRowanProjectsSandbox [
	^ RwAbstractV2Test _testRowanProjectsSandbox
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0000: commitMessage [
	"Create an initial project template with no packages -- basically an empty project.
		assume that github project already exists."

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0000: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0001: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0002: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0003: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom branchName title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core3'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor_004_b: specName
		branchName: branchName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core1'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0004: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0005: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0006: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0007: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0008: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0009: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0010: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0011: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom title |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0012: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0013: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0014: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0015: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0016: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0017: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0018: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0019: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0020: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0021: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0022: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0023: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0024: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0025: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {'Core'};
		groupNames:
				{'core'.
					'tests'};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0026: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
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

	| indexCard loadSpecification resolvedRepository specName postfix derivedFrom |
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

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectsHome: self projectsHome;
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: {projectName};
		groupNames: {};
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		comment: indexCard title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0027: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
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
RwTestProjectLibraryGenerator >> projectName [
	^projectName
]

{ #category : 'accessing' }
RwTestProjectLibraryGenerator >> projectName: object [
	projectName := object
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
