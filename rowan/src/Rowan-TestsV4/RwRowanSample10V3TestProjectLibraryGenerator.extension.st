Extension { #name : 'RwRowanSample10V3TestProjectLibraryGenerator' }

{ #category : '*rowan-testsv4' }
RwRowanSample10V3TestProjectLibraryGenerator >> genSpec_0006: commitMessage [
	"Start with spec_0003; part of recursive required project chain; RowanSample9V4 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V4; class hierarchy runs RowanSample9V4Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V4Class1"

	"https://github.com/GemTalk/Rowan/issues/701"
	"https://github.com/GemTalk/Rowan/issues/861"
	"https://github.com/GemTalk/Rowan/issues/953"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0006'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0003'.
	title := 'Start with spec_0003; part of recursive required project chain; RowanSample9V4 requires RowanSample10V3; RowanSample10V3 requires RowanSample11; RowanSample11 requires RowanSample12; RowanSamplle12 requires RowanSample9V4; class hierarchy runs RowanSample9V4Class2->RowanSample12Class1>RowanSample11Class1>RowanSample10V3Class1>RowanSample9V4Class1'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 6
		derivedFrom: derivedFrom
		comment: 'recursive required project chain')
		rowanIssues: #(701 861 953);
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
				requiredProjectName: 'RowanSample11' 
				revision: 'spec_0006'].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]
