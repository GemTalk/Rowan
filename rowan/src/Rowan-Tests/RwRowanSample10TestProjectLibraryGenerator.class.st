"
| commitComment |
RwRowanSample10Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample10') ensureDeleteAll.

commitComment := ''.
RwRowanSample10TestProjectLibraryGenerator new
	projectName: 'RowanSample10';
	projectUrl: 'git@github.com:dalehenrich/RowanSample10.git';
	preserveRowanSHA: false;
	primeIndexCardMap;
	preserveChangesOnGithub: false;
	genSpec_0000: commitComment;
	genSpec_0001: commitComment;
	yourself
"
Class {
	#name : 'RwRowanSample10TestProjectLibraryGenerator',
	#superclass : 'RwTestProjectLibraryGenerator',
	#category : 'Rowan-Tests'
}

{ #category : 'generators' }
RwRowanSample10TestProjectLibraryGenerator >> genSpec_0001: commitMessage [
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

{ #category : 'private' }
RwRowanSample10TestProjectLibraryGenerator >> primaryProjectName [
	^ 'RowanSample10'
]

{ #category : 'accessing' }
RwRowanSample10TestProjectLibraryGenerator >> projectReadmeIntro [
	"Intro in README"

	^ 'Sample project that is being used as an example required external project for use in RowanSample9.'
]
