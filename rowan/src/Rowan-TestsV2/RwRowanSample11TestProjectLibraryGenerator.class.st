"
| commitComment |
RwRowanSample9Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample11') ensureDeleteAll.

commitComment := ''.
RwRowanSample11TestProjectLibraryGenerator new
	projectName: 'RowanSample11';
	projectUrl: 'git@github.com:dalehenrich/RowanSample11.git';
	preserveRowanSHA: false;
	primeIndexCardMap;
	preserveChangesOnGithub: false;
	genSpec_0000: commitComment;
	genSpec_0001: commitComment;
	genSpec_0002: commitComment;
	yourself
"
Class {
	#name : 'RwRowanSample11TestProjectLibraryGenerator',
	#superclass : 'RwTestProjectLibraryGenerator',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample11TestProjectLibraryGenerator >> _addDefinitionsFor_0002: resolvedProject [
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

{ #category : 'generators' }
RwRowanSample11TestProjectLibraryGenerator >> genSpec_0001: commitMessage [
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
			'One class per package: RowanSample11-Core and RowanSample11-Tests packages ... External project for RowanSample9')
		rowanIssues: #();
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
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwRowanSample11TestProjectLibraryGenerator >> genSpec_0002: commitMessage [
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
		loadSpecification: resolvedRepository _loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : 'accessing' }
RwRowanSample11TestProjectLibraryGenerator >> projectReadmeIntro [
	"Intro in README"

	^ 'Sample project that is being used as an example required external project for use in RowanSample9.'
]
