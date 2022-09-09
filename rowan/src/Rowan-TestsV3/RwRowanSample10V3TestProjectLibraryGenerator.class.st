Class {
	#name : 'RwRowanSample10V3TestProjectLibraryGenerator',
	#superclass : 'RwTestProjectLibraryGeneratorV3',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample10V3TestProjectLibraryGenerator >> _addDefinitionsFor_0002: resolvedProject [
	"Start with spec_0001 and change the tests attribute to mytests"

	resolvedProject
		customConditionalAttributes: {'mytests'};
		addSubcomponentNamed: 'mytests/Tests' condition: 'mytests';
		addComponentNamed: 'mytests/Tests' toComponentNamed: 'Core';
		movePackageNamed:  projectName , '-Tests' toComponentNamed: 'mytests/Tests';
		removeComponentNamed: 'tests/Tests';
		yourself.
	^ resolvedProject
]

{ #category : 'private' }
RwRowanSample10V3TestProjectLibraryGenerator >> _addDefinitionsFor_0005: resolvedProject [
	"Start with spec_0001; add RowanSample9Class1>>foo and RowanSample9Class1 class>>bar extension methods ... that collide with existing packaged method in RowanSampe1 project when loaded on top of previously loaded RowanSample1:spec_0001"

	((resolvedProject
		addPackageNamed: projectName , '-Extensions'
		toComponentNamed: 'Core') addClassExtensionNamed: 'RowanSample9Class1')
		addInstanceMethod: 'foo "I stomp on existing method" ^2'
			protocol: 'accessing';
		addClassMethod: 'bar "I stomp on existing method" ^''bare'''
			protocol: 'accessing';
		yourself.
	^ resolvedProject
]

{ #category : 'generators' }
RwRowanSample10V3TestProjectLibraryGenerator >> genSpec_0001: commitMessage [
	"Create a bare bones package structure -Core and -Tests, the tests should validate the loaded state of project. Start with spec_0000 and add spec_0001 meat to the bones. This project is intended to be used as an external required project for RowanSample9"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0001'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Create a bare bones package structure -Core and -Tests, the tests should validate the loaded state of project. Start with spec_0000 and add spec_0001 meat to the bones. This project is intended to be used as an external required project for RowanSample9'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 1
		derivedFrom: derivedFrom
		comment:
			'One class per package: RowanSample10-Core and RowanSample10-Tests packages ... External project for RowanSample9')
		rowanIssues: #(668);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests' };
		comment: title;
		packageConvention: 'Rowan';
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0001: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwRowanSample10V3TestProjectLibraryGenerator >> genSpec_0002: commitMessage [
	"Start with spec_0001 and change the tests attribute to mytests"

	"https://github.com/dalehenrich/Rowan/issues/700"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0002'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001 and change the tests attribute to mytests.'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 2
		derivedFrom: derivedFrom
		comment:
			'Duplicate of spec_0001 with only a change in customConditionalAttribues')
		rowanIssues: #(700);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'mytests' . 'tests'};
		packageConvention: 'Rowan';
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0002: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwRowanSample10V3TestProjectLibraryGenerator >> genSpec_0003: commitMessage [
	"Start with spec_0001; part of linear required project chain; RowanSample9 requires RowanSample10; RowanSample10 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10Class1>RowanSample9Class1"

	"https://github.com/GemTalk/Rowan/issues/701"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0003'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001; part of linear required project chain; RowanSample9 requires RowanSample10; RowanSample10 requires RowanSample11; RowanSample11 requires RowanSample12; class hierarchy runs Object->RowanSample12Class1>RowanSample11Class1>RowanSample10Class1>RowanSample9Class1'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 3
		derivedFrom: derivedFrom
		comment: 'Duplicate of spec_0001 with only a change in superclass name')
		rowanIssues: #(701);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		packageConvention: 'Rowan';
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | 
			self
				_addDefinitionsFor_0067: resolvedProject
				requiredProjectName: 'RowanSample11' ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwRowanSample10V3TestProjectLibraryGenerator >> genSpec_0004: commitMessage [
	"Start with spec_0003; part of recursive required project chain; RowanSample9 requires RowanSample10; RowanSample10 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9; class hierarchy runs RowanSample9Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10Class1>RowanSample9Class1"

	"https://github.com/GemTalk/Rowan/issues/701"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0004'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0003'.
	title := 'Start with spec_0003; part of recursive required project chain; RowanSample9 requires RowanSample10; RowanSample10 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9; class hierarchy runs RowanSample9Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10Class1>RowanSample9Class1'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 4
		derivedFrom: derivedFrom
		comment: 'recursive required project chain')
		rowanIssues: #(701);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		packageConvention: 'Rowan';
		comment: title;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | 
			self
				_addDefinitionsFor_0068: resolvedProject
				requiredProjectName: 'RowanSample11' ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwRowanSample10V3TestProjectLibraryGenerator >> genSpec_0005: commitMessage [
	"Start with spec_0001; add RowanSample9Class1>>foo and RowanSample9Class1 class>>bar extension methods ... that collide with existing packaged method in RowanSampe1 project when loaded on top of previously loaded RowanSample1:spec_0001"

	"https://github.com/GemTalk/Rowan/issues/752"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0005'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0001'.
	title := 'Start with spec_0001; add RowanSample9Class1>>foo and RowanSample9Class1 class>>bar extension methods ... that collide with existing packaged method in RowanSampe1 project when loaded on top of previously loaded RowanSample1:spec_0001'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 5
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(545 752);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {'tests'};
		packageConvention: 'Rowan';
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0005: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'private' }
RwRowanSample10V3TestProjectLibraryGenerator >> primaryProjectName [
	^ 'RowanSample10'
]

{ #category : 'accessing' }
RwRowanSample10V3TestProjectLibraryGenerator >> projectReadmeIntro [
	"Intro in README"

	^ 'Sample project that is being used as an example required external project for use in RowanSample9.'
]
