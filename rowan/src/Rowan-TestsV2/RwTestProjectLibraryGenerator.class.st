"
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

RwTestProjectLibraryGenerator new
	projectName: 'RowanSample9';
	projectUrl: 'https://github.com/dalehenrich/RowanSample9';
	preserveChangesOnGithub: false;
	genSpec_0000: 'generate project definitions for specName_0000';
	genSpec_0001: 'generate project definitions for specName_0001';
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
RwTestProjectLibraryGenerator >> _createCard: postfix specName: specName title: title index: index comment: comment [
	^ RwTestProjectLibraryIndexCard new
		name: 'index_' , postfix;
		title: title;
		specName: specName;
		index: index;
		comment: comment;
		rowanIssues: {};
		gemstoneIssues: {};
		yourself
]

{ #category : 'private' }
RwTestProjectLibraryGenerator >> _createLoadSpecificatione: aString [
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
RwTestProjectLibraryGenerator >> _finishCommitAndPush: specName indexCard: indexCard loadSpecification: loadSpecification repositoryDefinition: repositoryDefinition commitMessage: commitMessage [
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
RwTestProjectLibraryGenerator >> _genSpecFor: specName loadSpecification: loadSpecification addDefinitions: addDefinitionsBlock [
	"Coming in the load spec revision is pointing at the desired starting 
		point --- the structure to _start_ with. Since we don't know whether the 
		repository has been cloned or not, we do a resolve, which will ensure
		that the repository is on the revision specifiied in the incoming load spec.
	Once we've resolved and read, we reset the revision of the load spec
		to point to the branch we want to write to. We wipe out the project
		roots, so we have a clean sarting point and then export the project
		definition and load spec."

	| repositoryDefinition projectDefinition oldPolicy |
	oldPolicy := loadSpecification repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checkout out in the repolicy"
	loadSpecification repositoryResolutionPolicy: #'strict'.
	loadSpecification resolve ]
		ensure: [ loadSpecification repositoryResolutionPolicy: oldPolicy ].
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
	projectDefinition := loadSpecification resolve.
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

	| indexCard loadSpecification repositoryDefinition specName postfix |
	postfix := '0000'.
	specName := 'spec_' , postfix.
	indexCard := self
		_createCard: postfix
		specName: specName
		title: 'Empty project with no packages'
		index: 0
		comment: ''.
	loadSpecification := (self _createLoadSpecificatione: self projectName)
		specName: specName;
		revision: 'master';
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

	| indexCard loadSpecification repositoryDefinition specName postfix |
	postfix := '0001'.
	specName := 'spec_' , postfix.
	indexCard := self
		_createCard: postfix
		specName: specName
		title: 'Bare bones package structure -Core and -Tests'
		index: 1
		comment: 'One class per package'.
	loadSpecification := (self _createLoadSpecificatione: self projectName)
		specName: specName;
		revision: 'spec_0000';
		gitUrl: self projectUrl;
		yourself.

	repositoryDefinition := self
		_genSpecFor: specName
		loadSpecification: loadSpecification
		addDefinitions: [ :projectDefinition | self _addDefinitionsFor_0001: projectDefinition ].

	self
		_finishCommitAndPush: specName
		indexCard: indexCard
		loadSpecification: loadSpecification
		repositoryDefinition: repositoryDefinition
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
