Class {
	#name : 'RwImplementedButNotSentTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwImplementedButNotSentTest >> _analyzeSent: sent andImplemented: implemented forClasses: classes [
	| env |
	env := 0.
	classes
		do: [ :aClass | 
			| cls |
			cls := aClass.
			2
				timesRepeat: [ 
					| mDict |
					mDict := cls _fullMethodDictEnv: env.
					mDict
						valuesDo: [ :method | 
							implemented ifNotNil: [ implemented add: method selector ].
							sent ifNotNil: [ sent addAll: method _selectorPool ] ].
					cls := cls class ] ]
]

{ #category : 'private' }
RwImplementedButNotSentTest >> _analyzeSent: sent selfSenders: selfSenders andImplemented: implemented forClassDefinition: classDef [
	| beh |
	beh := System myUserProfile objectNamed: classDef name.
	{(classDef instanceMethodDefinitions keys).
	(classDef classMethodDefinitions keys)}
		do: [ :selectors | 
			selectors
				do: [ :selector | 
					| sel meth selectorPool |
					sel := selector asSymbol.
					implemented ifNotNil: [ implemented add: sel ].
					meth := beh compiledMethodAt: sel.
					selectorPool := meth _selectorPool.
					(selectorPool includes: sel)
						ifTrue: [ selfSenders add: sel ].
					sent ifNotNil: [ sent addAll: selectorPool ] ].
			beh := beh class ]
]

{ #category : 'private' }
RwImplementedButNotSentTest >> _analyzeSent: sent selfSenders: selfSenders andImplemented: implemented forProjectNamed: projectName [
	| project |
	project := Rowan projectNamed: projectName.
	project packages
		do: [ :package | 
			((package name includesString: '-Test')
				or: [ 
					(package name includesString: '-Services-')
						or: [ 
							(package name includesString: 'JadeServer')
								or: [ 
									(package name includesString: 'AST-')
										or: [ 
											(package name includesString: 'GemStone-Interactions')
												or: [ package name includesString: '-Cypress-' ] ] ] ] ])
				ifFalse: [ 
					package _loadedPackage classDefinitions
						do: [ :def | 
							self
								_analyzeSent: sent
								selfSenders: selfSenders
								andImplemented: implemented
								forClassDefinition: def ].
					package _loadedPackage classExtensionDefinitions
						do: [ :def | 
							self
								_analyzeSent: sent
								selfSenders: selfSenders
								andImplemented: implemented
								forClassDefinition: def ] ] ]
]

{ #category : 'private' }
RwImplementedButNotSentTest >> _expectedSingleSenderMessages [
	"those messages that are expected to be implmented but not sent"

	| singleSenders |
	singleSenders := IdentitySet new.
	(self class allSelectors select: [ :ea | ea beginsWith: 'singleSenderSelectors' ])
		do: [ :selector | singleSenders addAll: (self perform: selector) ].
	^ singleSenders
]

{ #category : 'private' }
RwImplementedButNotSentTest >> _expectedUnsentMessages [
	"those messages that are expected to be implmented but not sent"

	| unSent |
	unSent := IdentitySet new.
	(self class allSelectors select: [ :ea | ea beginsWith: 'unsentSelectors' ])
		do: [ :selector | unSent addAll: (self perform: selector) ].
	^ unSent
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> singleSenderSelectorsGemStone [
	"These are selectors where there is a single sender in the image and the sending method is the same selector (apparent self sender).
		These are cases where there is more than one implementor .. we should be able to exclude these cases, but haven't done so yet ..."

	^ #(#'flattenOn:')
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> singleSenderSelectorsJadeite [
	"These are selectors where there is a single sender in the image and the sending method is the same selector (apparent self sender).
		These are cases where there is more than one implementor .. we should be able to exclude these cases, but haven't done so yet ..."

	^ #(#'projectServiceClass' #'methodServiceClass' #'browserServiceClass' #'answeringServiceClass')
]

{ #category : 'tests' }
RwImplementedButNotSentTest >> testImplementedButNotSent [
	"find messages within the Rowan code base that are implemented but not sent"

	| sent implemented implementedNotSent singleSenders selfSenders singleImplementors sentBySelfOnly |
	sent := IdentityBag new.
	selfSenders := IdentitySet new.
	implemented := IdentityBag new.

	self
		_analyzeSent: nil
		selfSenders: selfSenders
		andImplemented: implemented
		forProjectNamed: 'Rowan'.
	self
		_analyzeSent: sent
		andImplemented: nil
		forClasses: ClassOrganizer new classes.

	singleImplementors := implemented
		select: [ :each | (implemented occurrencesOf: each) = 1 ].
	singleSenders := IdentitySet new.
	sent
		do: [ :each | 
			"if there is only one sender and one implementor of the same selector, make sure that it is not a self send"
			((sent occurrencesOf: each) = 1
				and: [ (singleImplementors includes: each) and: [ selfSenders includes: each ] ])
				ifTrue: [ singleSenders add: each ] ].
	sentBySelfOnly := singleSenders - self _expectedSingleSenderMessages.
	implementedNotSent := implemented asIdentitySet - sent asIdentitySet
		- self _expectedUnsentMessages.

	self assert: sentBySelfOnly isEmpty.
	self assert: implementedNotSent isEmpty
]

{ #category : 'tests' }
RwImplementedButNotSentTest >> testVerifySpecialCasesStillRelevant [
	"identify that the special cases selectors represented implemented methods --- just to keep things tidy"

	| implemented expectedUnsentMessages unimplemented |
	implemented := IdentitySet new.
	self
		_analyzeSent: nil
		andImplemented: implemented
		forClasses: ClassOrganizer new classes.
	expectedUnsentMessages := self _expectedUnsentMessages asIdentitySet.
	unimplemented := expectedUnsentMessages - implemented.
	self assert: unimplemented isEmpty
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsForIssue_652 [
	"see https://github.com/GemTalk/Rowan/issues/652"

	^ #( #'readClassFile:' #'readClassFile:projectName:packageName:' #'readClassDirectory:' #'readClassDirectory:projectName:packageName:')
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsFromBuildScripts [
	"These are selectors that are used in GemStone build scripts, thus no senders in image"

	"worth a sanity check now and then"

	^ #(#'_removeSubclassesDisallowed')
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsThatNeedMoreSrutiny [
	"These are selectors that should have test coverage at a minimum OR removed"

	"the list should be empty in the long term, but if a method is deemed to be needed in short term, this is the right place"

	^ #()
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsThatShouldBeSent [
	"These are selectors that should have test coverage at a minimum OR removed"

	"using this list while wading through the current set of implemented but not sent methods ... this should be empty when all test cases are covered"

	^ #(#'addClass:inPackageNamed:inProjectNamed:' #'addClassNamed:super:instvars:classinstvars:classvars:category:comment:pools:' #'addClassNamed:super:instvars:classinstvars:classvars:category:comment:pools:type:' #'addRowanSymbolDictionariesToPersistentSymbolList' #'addRowanSymbolDictionariesToTransientSymbolList' #'adoptProjectFromUrl:projectsHome:' #'adoptProjectFromUrl:readOnlyDiskUrl:projectsHome:' #'aliases:' #'asDefinitionSet' #'auditAll' #'auditForPackageNamed:' #'auditForProjectsNamed:' #'clas' #'classCreationTemplateForClass:' #'classesModificationAndPackageModificationDo:' #'conservativeMigration' #'currentPackage' #'currentProject' #'decrementMajorVersion' #'decrementMinorVersion' #'decrementMinorVersionNumber' #'decrementPatchVersion' #'defaultAutomaticClassInitializationBlackList' #'definitionsForMethod:inClassNamed:isMeta:ifFound:ifAbsent:' #'deletededClass:' #'deletededClassExtension:' #'deletededPackage:' #'doitDict:' #'exportTopazFormatTo:' #'exposeRowanToolsSymbolDictionary' #'gitlogFull:limit:gitRepoDirectory:' #'gitpullIn:' #'incrementMajorVersion' #'incrementMinorVersion' #'incrementMinorVersionNumber' #'incrementPatchVersion' #'instanceMigrator:' #'loadedPackages:' #'loadSymbolList:' #'newForClassNamed:super:instvars:category:' #'parseString:forReader:' #'pathParts:isAbsolute:' #'remote:' #'removeClassNamed:ifAbsent:' #'requiredProjectNames' #'rwByteSubclass:classVars:classInstVars:poolDictionaries:category:packageName:options:' #'rwIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:category:constraints:options:' #'rwIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:category:packageName:constraints:options:' #'rwIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:category:packageName:options:' #'rwSubclass:instVarNames:classVars:classInstVars:poolDictionaries:category:packageName:options:' #'testSuiteForProjectsNamed:' #'topazFileFooter:' #'validateVersionNumber:against:' #'_alternateImageClass' )
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsThatWillBeSent [
	"These are selectors that could/should have test coverage, but are used for features that aren't implemented "

	"Use this list for selectors that we intend to keep"

	^ #(#'mercurialUrl:' #'svnUrl:' #'revert' #'revert:')
]
