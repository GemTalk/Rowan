Class {
	#name : 'RwImplementedButNotSentTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwImplementedButNotSentTest >> _analyzeSent: sent andImplemented: implemented forClassDefinition: classDef [
	| beh |
	beh := System myUserProfile objectNamed: classDef name.
	{(classDef instanceMethodDefinitions keys).
	(classDef classMethodDefinitions keys)}
		do: [ :selectors | 
			selectors
				do: [ :selector | 
					| sel |
					sel := selector asSymbol.
sel == #'
asOop' ifTrue: [ self halt ].
					implemented ifNotNil: [ implemented add: sel ].
					sent
						ifNotNil: [ 
							| meth |
							meth := beh compiledMethodAt: sel.
							sent addAll: meth _selectorPool ] ].
			beh := beh class ]
]

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
RwImplementedButNotSentTest >> _analyzeSent: sent andImplemented: implemented forProjectNamed: projectName [
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
						do: [ :def | self _analyzeSent: sent andImplemented: implemented forClassDefinition: def ].
					package _loadedPackage classExtensionDefinitions
						do: [ :def | self _analyzeSent: sent andImplemented: implemented forClassDefinition: def ] ] ]
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

{ #category : 'tests' }
RwImplementedButNotSentTest >> testImplementedButNotSent [
	"find messages within the Rowan code base that are implemented but not sent"

	| sent implemented implementedNotSent |
	sent := IdentitySet new.
	implemented := IdentitySet new.
	self _analyzeSent: nil andImplemented: implemented forProjectNamed: 'Rowan'.
	self
		_analyzeSent: sent
		andImplemented: nil
		forClasses: ClassOrganizer new classes.
	implementedNotSent := implemented - sent - self _expectedUnsentMessages.
	self assert: implementedNotSent isEmpty
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsThatShouldBeRemoved [
	"These are selectors that should be removed"

	"using this list to manage the list of methods that I think should be removed ... empty this list when done (rerun tests, because the removed methods might be hiding additional orphan messages"

	^ #(#'addClasses:' #'addClassExtension:inPackageNamed:inProjectNamed:' #'addClassExtensions:' #'addCreatedClassesAndVersionsTo:' #'addExtensionSessionMethods:catDict:for:toPackageNamed:' #'addMovedDeletedMethod:for:protocol:toPackageNamed:' #'addMovedDeletedMethod:for:protocol:toPackageNamed:implementationClass:' #'addOrUpdatePackage:' #'addPackagesNamed:toProjectNamed:' #'addRecompiledSessionMethodMethod:' #'addSimpleComponentNamed:condition:groupName:comment:' #'applyModification_V2:instanceMigrator:' #'associations' #'asSpecification' #'auditV2' #'browserV2' #'checkoutSpecUrl:' #'classIsMeta:' #'classPatchesInReverseHierarchyOrder:tempSymbolList:' #'compileUsingNewClasses:andExistingClasses:' #'compileUsingNewClasses:andExistingClassSymbolList:' #'conditionalPackageMapSpecs:' #'conditionalPackageMapSpecsAt:ifAbsent:' #'conditionalPackageMapSpecsAtGemStoneUserId:andPackageName:' #'createClassesFromWorklist:andClassesByName:' #'createLoadedPackageFromDefinition:' #'createLoadedPackageNamed:inProjectNamed:' #'createLoadedPackageNamed:properties:' #'createLoadedPackageNamed:properties:implementationClass:' #'deleteClasses:' #'deleteClassFromPackage:implementationClass:' #'deleteClassNamedFromPackage:' #'deleteCompiledMethod:from:implementationClass:' #'deleteMethodNewClasses:andExistingClasses:' #'deleteMethodNewClasses:andExistingClassSymbolList:' #'deleteMethod:for:' #'deleteMovedLoadedMethod:for:implementationClass:' #'deleteMovedMethod:for:implementationClass:' #'deleteNewVersionMethodNewClasses:andExistingClasses:' #'deleteNewVersionMethodNewClasses:andExistingClassSymbolList:' #'dir' #'dirPath' #'doGitCommit:' #'doGitCommitLog:')
]

{ #category : 'special cases' }
RwImplementedButNotSentTest >> unsentSelectorsThatShouldBeSent [
	"These are selectors that should have test coverage at a minimum OR removed"

	"using this list while wading through the current set of implemented but not sent methods ... this should be empty when all test cases are covered"

	^ #(#'addClass:inPackageNamed:inProjectNamed:' #'addClassNamed:super:instvars:classinstvars:classvars:category:comment:pools:' #'addClassNamed:super:instvars:classinstvars:classvars:category:comment:pools:type:' #'addRowanSymbolDictionariesToPersistentSymbolList' #'addRowanSymbolDictionariesToTransientSymbolList' #'adoptProjectFromUrl:projectsHome:' #'adoptProjectFromUrl:readonlyDiskUrl:projectsHome:' #'aliases:' #'asDefinitionSet' #'auditAll' #'auditForPackageNamed:' #'auditForProjectsNamed:' #'auditProjectsNamed:' #'clas' #'classCreationTemplateForClass:' #'classesModificationAndPackageModificationDo:' #'conservativeMigration' #'currentPackage' #'currentProject' #'decrementMajorVersion' #'decrementMinorVersion' #'decrementMinorVersionNumber' #'decrementPatchVersion' #'defaultAutomaticClassInitializationBlackList' #'definitionsForMethod:inClassNamed:isMeta:ifFound:ifAbsent:' #'deletededClass:' #'deletededClassExtension:' #'deletededPackage:' #'doGitPull:' #'doGitPush:' #'doitDict:')
]
