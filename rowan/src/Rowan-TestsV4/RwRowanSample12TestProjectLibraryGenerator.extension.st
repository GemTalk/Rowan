Extension { #name : 'RwRowanSample12TestProjectLibraryGenerator' }

{ #category : '*rowan-testsv4' }
RwRowanSample12TestProjectLibraryGenerator >> _addDefinitionsFor_0007: resolvedProject [
	"Start with spec_0000. The class RowanSample12ConcreteClass1 has a superclass AbstractRowanSample9V4BridgeClass1 from the project RowanSample9. This project requires RowanSample9V4:spec_0069. The method #foo is defined in RowanSample12ConcreteClass1."

	| className packageName requiredLoadSpec requiredSpecName requiredProjectName componentName requiredRevision requiredComponentName |
	componentName := 'Core'.
	packageName := projectName , '-' , componentName.
	className := projectName , 'ConcreteClass1'.
	requiredProjectName := 'RowanSample9V4'.
	requiredComponentName := 'Bridge'.
	requiredSpecName := requiredProjectName , '_' , requiredComponentName.
	requiredRevision := 'spec_0069'.

	resolvedProject
		addPackagesNamed: {packageName} toComponentNamed: componentName;
		removeComponentNames: {'Tests'};
		removeComponentNamed: 'tests/Tests';
		yourself.
	(resolvedProject componentNamed: componentName)
		addProjectNamed: requiredSpecName.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Abstract', requiredProjectName, 'BridgeClass1'
		instvars: #()
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.

	resolvedProject projectsRoot ensureCreateDirectory.
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredSpecName;
		projectName: requiredProjectName;
		revision: requiredRevision;
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName , '.git';
		componentNames: {requiredComponentName};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: RwRowanSample12Test _sampleSymbolDictionaryName1;
		yourself.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.

	^ resolvedProject
]

{ #category : '*rowan-testsv4' }
RwRowanSample12TestProjectLibraryGenerator >> _addDefinitionsFor_0008: resolvedProject [
	"Start with spec_0000. The class RowanSample12ConcreteClass1 subclasses AbstractRowanSample9V4ParentClass1 in RowanSample9V4:spec_0069, component Parent. RowanSample9V4. RowanSample9V4:spec_0069 componant Parent has the test. This spec requires RowanSample9V4:spec_0069."

	| className packageName requiredLoadSpec requiredSpecName requiredProjectName componentName requiredRevision requiredComponentName |
	componentName := 'Core'.
	packageName := projectName , '-' , componentName.
	className := projectName , 'ConcreteClass1'.
	requiredProjectName := 'RowanSample9V4'.
	requiredComponentName := 'Parent'.
	requiredSpecName := requiredProjectName , '_' , requiredComponentName.
	requiredRevision := 'spec_0069'.

	resolvedProject
		addPackagesNamed: {packageName} toComponentNamed: componentName;
		removeComponentNames: {'Tests'};
		removeComponentNamed: 'tests/Tests';
		yourself.
	(resolvedProject componentNamed: componentName)
		addProjectNamed: requiredSpecName.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Abstract', requiredProjectName, 'ParentClass1'
		instvars: #()
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.

	resolvedProject projectsRoot ensureCreateDirectory.
	requiredLoadSpec := RwLoadSpecificationV2 new
		specName: requiredSpecName;
		projectName: requiredProjectName;
		revision: requiredRevision;
		gitUrl: 'git@github.com:dalehenrich/' , requiredProjectName , '.git';
		componentNames: {requiredComponentName};
		customConditionalAttributes: {'tests'};
		gemstoneSetDefaultSymbolDictNameTo: RwRowanSample12Test _sampleSymbolDictionaryName1;
		yourself.
	requiredLoadSpec exportTo: resolvedProject projectsRoot.

	^ resolvedProject
]

{ #category : '*rowan-testsv4' }
RwRowanSample12TestProjectLibraryGenerator >> genSpec_0007: commitMessage [
	"Start with spec_0000. The class RowanSample12ConcreteClass1 has a superclass AbstractRowanSample9BridgeClass1 from the project RowanSample9V4. This project requires RowanSample9V4:spec_0069. The method #foo is defined in RowanSample12ConcreteClass1. Based on spec_0006"

	"https://github.com/GemTalk/Rowan/issues/705"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0007'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Start with spec_0000. The class RowanSample12ConcreteClass1 has a superclass AbstractRowanSample9BridgeClass1 from the project RowanSample9V4. This project requires RowanSample9V4:spec_0069. The method #foo is defined in RowanSample12ConcreteClass1. Based on spec_0006'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 7
		derivedFrom: derivedFrom
		comment: 'jigsaw puzzle class relationships')
		rowanIssues: #(705);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {};
		packageConvention: 'Rowan';
		comment: title;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0007: resolvedProject ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]

{ #category : '*rowan-testsv4' }
RwRowanSample12TestProjectLibraryGenerator >> genSpec_0008: commitMessage [
	"Start with spec_0000. The class RowanSample12ConcreteClass1 subclasses AbstractRowanSample9ParentClass1 in RowanSample9:spec_0069, component Parent. RowanSample9V4. RowanSample9V4:spec_0069 componant Parent has the test. This spec requires RowanSample9V4:spec_0069. Based on spec_0005"

	"https://github.com/GemTalk/Rowan/issues/705"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard definedProject resolvedRepository specName postfix derivedFrom title |
	postfix := '0008'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.
	title := 'Start with spec_0000. The class RowanSample12ConcreteClass1 subclasses AbstractRowanSample9ParentClass1 in RowanSample9:spec_0069, component Parent. RowanSample9V4. RowanSample9V4:spec_0069 componant Parent has the test. This spec requires RowanSample9V4:spec_0069. Based on spec_0005'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: title
		index: 5
		derivedFrom: derivedFrom
		comment: 'jigsaw puzzle class relationships')
		rowanIssues: #(705);
		yourself.
	definedProject := (self _createDefinedProject: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		specComponentNames: {'Core'};
		customConditionalAttributes: {};
		packageConvention: 'Rowan';
		comment: title;
		yourself.

	resolvedRepository := self
		_genSpecFor: specName
		definedProject: definedProject
		addDefinitions: [ :resolvedProject | self _addDefinitionsFor_0005: resolvedProject requiredProjectName: 'RowanSample9V4' ].

	self
		_finishCommitAndPush: specName
		loadSpecification: resolvedRepository loadSpecification
		indexCard: indexCard
		derivedFrom: (derivedFrom copyReplaceAll: 'spec' with: 'index')
		resolvedRepository: resolvedRepository
		commitMessage: commitMessage
]
