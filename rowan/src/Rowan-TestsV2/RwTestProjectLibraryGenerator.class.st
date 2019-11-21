"
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

RwTestProjectLibraryGenerator new
	projectName: 'RowanSample9';
	projectUrl: 'https://github.com/dalehenrich/RowanSample9';
	preserveChangesOnGithub: true;
	genSpec_0000: 'generate project definitions for spec_0000';
	genSpec_0001: 'generate project definitions for spec_0001';
	genSpec_0002: 'generate project definitions for spec_0002';
	genSpec_0003: 'generate project definitions for spec_0003';
	genSpec_0004: 'generate project definitions for spec_0004';
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
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0000: projectDefinition [
	projectDefinition
		comment: 'spec_0000 definitions';
		addComponentNamed: 'Core'
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						add: 'tests' -> {'core'};
						yourself)
			comment: 'Primary component used for loading the core classes.';
		yourself.
	^ projectDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0001: projectDefinition [
	| className packageName |
	projectDefinition
		addPackagesNamed: {(projectName , '-Core')}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
		yourself.
	packageName := projectName , '-Core'.
	className := projectName , 'Class1'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-Tests'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	^ projectDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0002: projectDefinition [
	"RowanSample4 basic package and class structure"

	| packageName1 packageName2 packageName3 packageName4 packageName5 className gemstoneClassName |
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	projectDefinition
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
	((projectDefinition packageNamed: packageName1)
		addClassNamed: className
		super: 'Object'
		instvars: #()
		category: packageName1
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		addClassMethod: 'bar ^''bar''' protocol: 'accessing';
		yourself.
	gemstoneClassName := 'RowanSample4IdentityKeyValueDictionary'.
	((projectDefinition packageNamed: packageName2)
		addClassNamed: gemstoneClassName
		super: 'IdentityKeyValueDictionary'
		instvars: #()
		category: packageName2
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	((projectDefinition packageNamed: packageName3)
		addClassExtensionNamed: gemstoneClassName)
		addClassMethod: 'bar ^''bar''' protocol: '*' , packageName3 asLowercase;
		yourself.
	((projectDefinition packageNamed: packageName4)
		addClassNamed: projectName , 'GemStoneTestCase'
		super: 'TestCase'
		category: packageName4
		comment: 'I test the GemStone class')
		addInstanceMethod: 'test1  self assert: ' , gemstoneClassName , ' new foo = 1'
			protocol: 'tests';
		addInstanceMethod: 'test2  self assert: ' , gemstoneClassName , ' bar = ''bar'''
			protocol: 'tests';
		yourself.
	((projectDefinition packageNamed: packageName5)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName5
		comment: 'I test the Core class')
		addInstanceMethod: 'test1  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		addInstanceMethod: 'test2  self assert: ' , className , ' bar = ''bar'''
			protocol: 'tests';
		yourself.
	^ projectDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0003: projectDefinition [
	"primer for https://github.com/dalehenrich/Rowan/issues/230"

	| packageName1  className  |
	packageName1 := projectName , '-' , 'Core'.
	className := projectName , 'Class1'.
	((projectDefinition packageNamed: packageName1)
		classDefinitionNamed: className)
		gs_options: #(#'instancesInvariant');
		yourself.
	^ projectDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0004_b: projectDefinition [
	| userId |
	userId := 'SystemUser'.
	(projectDefinition
		addComponentNamed: 'Core3'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core1 package')
		componentNames: #('Core1');
		yourself.
	projectDefinition
		addPackageNamed: projectName , '-Core1'
			toComponentNamed: 'Core3'
			withConditions: {'common'}
			andGroupName: 'core';
		yourself.
	^ self _addPackageCore1DefinitionsFor_0004: projectDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _addDefinitionsFor_0004: projectDefinition [
	| className packageName userId |
	userId := projectDefinition _gemstoneAllUsersName.
	(projectDefinition
		addComponentNamed: 'Core1'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core1 package')
		componentNames: #('Core2');
		yourself.
	(projectDefinition
		addComponentNamed: 'Core2'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core2 package')
		componentNames: #('Core1');
		yourself.
	projectDefinition
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
	self _addPackageCore1DefinitionsFor_0004: projectDefinition.
	packageName := projectName , '-Core2'.
	className := projectName , 'Class2'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-Tests'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	^ projectDefinition
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
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _createLoadSpecification: aString [
	^ RwProjectLoadSpecificationV2 new
		projectName: aString;
		projectsHome: self projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _doCommitAndPush: commitMessage repositoryDefinition: repositoryDefinition branchName: branchName [
	| somethingToCommit |
	somethingToCommit := true.
	[ repositoryDefinition commit: commitMessage ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex interaction message = 'Nothing to commit'
				ifFalse: [ ex pass ].
			somethingToCommit := false.
			ex response: nil ].
	(somethingToCommit and: [ self preserveChangesOnGithub ])
		ifTrue: [ 
			"only push if we really want to preserve the changes during this run and something has changed"
			repositoryDefinition push: repositoryDefinition remote branch: branchName ]
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _finishCommitAndPush: specName indexCard: indexCard derivedFrom: derivedFrom loadSpecification: loadSpecification repositoryDefinition: repositoryDefinition commitMessage: commitMessage [
	| derivedIndexCardFile |
	derivedFrom
		ifNotNil: [ 
			derivedIndexCardFile := loadSpecification repositoryRoot / derivedFrom , 'ston'.
			derivedIndexCardFile exists
				ifTrue: [ derivedIndexCardFile delete ] ].
	indexCard exportTo: loadSpecification repositoryRoot.

	self
		_doCommitAndPush: commitMessage
		repositoryDefinition: repositoryDefinition
		branchName: specName.

	repositoryDefinition checkout: 'master'.	"write the index card and spec on the master branch"
	repositoryDefinition pull: repositoryDefinition remote branch: 'master'.

	(loadSpecification repositoryRoot / 'indexCards') ensureCreateDirectory.
	indexCard exportTo: loadSpecification repositoryRoot / 'indexCards'.
	(loadSpecification repositoryRoot / 'specs') ensureCreateDirectory.
	loadSpecification exportTo: loadSpecification repositoryRoot / 'specs'.

	self
		_doCommitAndPush: commitMessage
		repositoryDefinition: repositoryDefinition
		branchName: 'master'
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _finishCommitAndPush: specName indexCard: indexCard loadSpecification: loadSpecification repositoryDefinition: repositoryDefinition commitMessage: commitMessage [
	^ self
		_finishCommitAndPush: specName
		indexCard: indexCard
		derivedFrom: nil
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
		commitMessage: commitMessage
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor_004_b: specName branchName: branchName loadSpecification: loadSpecification addDefinitions: addDefinitionsBlock [
	"Create a second set of specs/components in an existing branch"

	| repositoryDefinition projectDefinition |
	projectDefinition := loadSpecification resolveStrict.

	repositoryDefinition := loadSpecification repositoryDefinition.
	(repositoryDefinition branches includes: branchName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			repositoryDefinition checkout: branchName.
			repositoryDefinition pull: repositoryDefinition remote branch: branchName ]
		ifFalse: [ self error: 'Expected branch ' , branchName printString , ' to already exist' ].

	loadSpecification revision: branchName.	"update the revision to match the current branch, since we want the revision set correctly when exported"
	addDefinitionsBlock value: projectDefinition.

	projectDefinition exportComponents.	"write the component directory structure"
	loadSpecification export.

	^ repositoryDefinition
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _genSpecFor_004_b: specName loadSpecification: loadSpecification addDefinitions: addDefinitionsBlock [
	"Create a second set of specs/components in an existing branch"

	| repositoryDefinition projectDefinition |
	projectDefinition := loadSpecification resolveStrict.

	repositoryDefinition := loadSpecification repositoryDefinition.
	(repositoryDefinition branches includes: specName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			repositoryDefinition checkout: specName.
			repositoryDefinition pull: repositoryDefinition remote branch: specName ]
		ifFalse: [ 
			"create the branch"
			repositoryDefinition createBranch: specName ].

	loadSpecification revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"
	addDefinitionsBlock value: projectDefinition.

	loadSpecification projectRoots
		do: [ :rootDir | 
			"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
			rootDir ensureDeleteAll ].

	projectDefinition export.	"write the rowan directory structure"
	loadSpecification export.

	^ repositoryDefinition
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

	| repositoryDefinition projectDefinition |
	projectDefinition := loadSpecification resolveStrict.

	repositoryDefinition := loadSpecification repositoryDefinition.
	(repositoryDefinition branches includes: specName)
		ifTrue: [ 
			"branch exists, so we'll check it out and make the necessary mods"
			repositoryDefinition checkout: specName.
			repositoryDefinition pull: repositoryDefinition remote branch: specName ]
		ifFalse: [ 
			"create the branch"
			repositoryDefinition createBranch: specName ].

	loadSpecification revision: specName.	"update the revision to match the current branch, since we want the revision set correctly when exported"
	addDefinitionsBlock value: projectDefinition.

	loadSpecification projectRoots
		do: [ :rootDir | 
			"eliminate the rowan directory structure, so it can be created afresh ... if it already exists"
			rootDir ensureDeleteAll ].

	projectDefinition export.	"write the rowan directory structure"
	loadSpecification export.

	^ repositoryDefinition
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

	| indexCard loadSpecification repositoryDefinition specName postfix derivedFrom |
	postfix := '0000'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.

	indexCard := self
		_createCard: postfix
		specName: specName
		title: 'Empty project with no packages'
		index: 0
		derivedFrom: derivedFrom
		comment: ''.
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		yourself.

	repositoryDefinition := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0000: projectDefinition ].

	self
		_finishCommitAndPush: specName
		indexCard: indexCard
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0001: commitMessage [
	"Create a bare bones package structure -Core and -Tests, the tests should validate 
		the loaded state of project. Start with spec_0000 and add spec_0001 meat to the bones"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard loadSpecification repositoryDefinition specName postfix derivedFrom |
	postfix := '0001'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.

	indexCard := self
		_createCard: postfix
		specName: specName
		title: 'Bare bones package structure -Core and -Tests'
		index: 1
		derivedFrom: derivedFrom
		comment: 'One class per package'.
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		yourself.

	repositoryDefinition := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0001: projectDefinition ].

	self
		_finishCommitAndPush: specName
		indexCard: indexCard
		derivedFrom:  (derivedFrom copyReplaceAll: 'spec' with: 'index')
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0002: commitMessage [
	"RowanSample4 - basic project definition"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard loadSpecification repositoryDefinition specName postfix derivedFrom |
	postfix := '0002'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0000'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: 'RowanSample4 basic project definition'
		index: 2
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #();
		yourself.
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		yourself.

	repositoryDefinition := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0002: projectDefinition ].

	self
		_finishCommitAndPush: specName
		indexCard: indexCard
		derivedFrom:  (derivedFrom copyReplaceAll: 'spec' with: 'index')
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0003: commitMessage [
	"RowanSample4 - primer project definition"

	"The method is idempotent with respect to the branches involved, UNLESS something
		has explicitly changed within the model or the disk format of artefacts."

	| indexCard loadSpecification repositoryDefinition specName postfix derivedFrom |
	postfix := '0003'.
	specName := 'spec_' , postfix.
	derivedFrom := 'spec_0002'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: 'RowanSample4 primer project definition'
		index: 3
		derivedFrom: derivedFrom
		comment: 'RowanSample9Class1 has instancesInvariant option set')
		rowanIssues: #(230);
		yourself.
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		yourself.

	repositoryDefinition := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0003: projectDefinition ].

	self
		_finishCommitAndPush: specName
		indexCard: indexCard
		derivedFrom:  (derivedFrom copyReplaceAll: 'spec' with: 'index')
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
		commitMessage: commitMessage
]

{ #category : 'generators' }
RwTestProjectLibraryGenerator >> genSpec_0004_b: commitMessage [
	"https://github.com/GemTalk/Rowan/issues/532"

	"The method is NOT idempotent with respect to the branches involved. It must be run whenever genSpec_0004 is run.
		Right now it is called from genSpec_0004_b, so explicit top-level call is not necessary."

	| indexCard loadSpecification repositoryDefinition specName postfix derivedFrom branchName |
	postfix := '0004_b'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.
	branchName := 'spec_0004'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title:
			'Create inconsistency in component definitions ... duplicate package error expected on load'
		index: 4
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(532);
		yourself.
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: #('Core3');
		yourself.

	repositoryDefinition := self
		_genSpecFor_004_b: specName
		branchName: branchName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0004_b: projectDefinition ].

	self
		_finishCommitAndPush: branchName
		indexCard: indexCard
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
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

	| indexCard loadSpecification repositoryDefinition specName postfix derivedFrom |
	postfix := '0004'.
	specName := 'spec_' , postfix.
	derivedFrom := 'master'.

	indexCard := (self
		_createCard: postfix
		specName: specName
		title: 'Reconcile inconsistencies for storage of package-specific properties'
		index: 4
		derivedFrom: derivedFrom
		comment: '')
		rowanIssues: #(500 531);
		yourself.
	loadSpecification := (self _createLoadSpecification: self projectName)
		specName: specName;
		revision: derivedFrom;
		gitUrl: self projectUrl;
		componentNames: #('Core1');
		yourself.

	repositoryDefinition := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0004: projectDefinition ].

	self
		_finishCommitAndPush: specName
		indexCard: indexCard
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
		commitMessage: commitMessage.

	self genSpec_0004_b: commitMessage , '_b'
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
