Class {
	#name : 'RwProjectSpecificationV2',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectName',
		'componentsPath',
		'packagesPath',
		'projectsPath',
		'specsPath',
		'projectsHome',
		'repoType',
		'gitUrl',
		'diskUrl',
		'mercurialUrl',
		'svnUrl',
		'revision',
		'comment',
		'platformProperties',
		'repositoryResolutionPolicy',
		'packageFormat',
		'packageConvention',
		'repositoryDefinition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwProjectSpecificationV2 >> _checkAndUpdateRepositoryRevision [
	^ self repositoryDefinition checkAndUpdateRepositoryRevision: self
]

{ #category : 'private' }
RwProjectSpecificationV2 >> _checkProjectDirectoryStructure [
	"answer true if the basic project directory structure is present"

	self projectRoots
		do: [ :fileRef | 
			fileRef exists
				ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> _platformProperties [
	"direct access to IV ... used by ="

	^ platformProperties
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> _repoType [
	"direct access to IV ... used by ="

	^ repoType
]

{ #category : 'actions' }
RwProjectSpecificationV2 >> _resolve [
	"answer true if attaching to an existing repository"

	^ self repositoryDefinition resolve
		ifTrue: [ 
			self _checkAndUpdateRepositoryRevision.
			self _checkProjectDirectoryStructure ]
		ifFalse: [ false ]
]

{ #category : 'private' }
RwProjectSpecificationV2 >> _validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| repoUrls |
	#(#'componentsPath' #'packagesPath' #'projectsPath' #'specsPath' #'projectName' #'specName')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	repoUrls := {}.
	#(#'gitUrl' #'diskUrl' #'mercurialUrl' #'svnUrl')
		do: [ :messageName | (self perform: messageName) ifNotNil: [ repoUrls add: messageName asString ] ].
	repoUrls size > 1
		ifTrue: [ Error signal: 'Only one of (gitUrl diskUrl mercurialUrl svnUrl) must be be set' ].
	(repoUrls size = 0 or: [ repoUrls = #('diskUrl') ])
		ifTrue: [ 
			self revision
				ifNotNil: [ :rev | 
					Error
						signal:
							'Invalid revision ' , rev printString
								, '. Should be nil for disk-based repository' ] ]
		ifFalse: [ 
			self gitUrl asRwUrl scheme ~= 'file'
				ifTrue: [ 
					Error
						signal:
							'The instance variable ''revision'' must be set for the'
								, (repoUrls at: 1) asString printString ] ].
	self platformProperties
		keysAndValuesDo: [ :platformName :userIdMap | 
			(RwAbstractProjectLoadComponentV2 _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString , ' in platform properties' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatformUserIdMap: userIdMap ] ].
	^ true
]

{ #category : 'private' }
RwProjectSpecificationV2 >> _validateGemStonePlatformUserIdMap: userIdMap [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| expectedPropertyMap |
	expectedPropertyMap := Dictionary new
		add: #'defaultSymbolDictName' -> CharacterCollection;
		add: #'defaultMethodEnv' -> SmallInteger;
		add: #'defaultUseSessionMethodsForExtensions' -> Boolean;
		yourself.
	userIdMap
		keysAndValuesDo: [ :userId :platformPropertiesMap | 
			platformPropertiesMap
				keysAndValuesDo: [ :propertyKey :propertyValue | 
					(expectedPropertyMap includesKey: propertyKey)
						ifTrue: [ 
							| expectedClass |
							expectedClass := expectedPropertyMap at: propertyKey.
							(propertyValue isKindOf: expectedClass)
								ifFalse: [ 
									Error
										signal:
											'Value of property (' , propertyKey printString , '->'
												, propertyValue printString , ') is expected to be class '
												, expectedClass name asString printString , ' not class '
												, propertyValue class name asString printString ] ]
						ifFalse: [ Error signal: 'Unknown platform property key ' , propertyKey printString ] ] ]
]

{ #category : 'comparing' }
RwProjectSpecificationV2 >> = anObject [
	| lazyEqual |
	^ self specName = anObject specName
		and: [ 
			self projectName = anObject projectName
				and: [ 
					self projectAlias = anObject projectAlias
						and: [ 
							self componentsPath = anObject componentsPath
								and: [ 
									self packagesPath = anObject packagesPath
										and: [ 
											self projectsPath = anObject projectsPath
												and: [ 
													self specsPath = anObject specsPath
														and: [ 
															self projectsHome = anObject projectsHome
																and: [ 
																	lazyEqual := self _repoType = anObject _repoType
																		or: [ self repoType = anObject repoType ].
																	lazyEqual
																		and: [ 
																			self gitUrl = anObject gitUrl
																				and: [ 
																					self diskUrl = anObject diskUrl
																						and: [ 
																							self mercurialUrl = anObject mercurialUrl
																								and: [ 
																									self svnUrl = anObject svnUrl
																										and: [ 
																											self revision = anObject revision
																												and: [ 
																													self comment = anObject comment
																														and: [ 
																															self componentNames asArray sort = anObject componentNames asArray sort
																																and: [ 
																																	self groupNames asArray sort = anObject groupNames asArray sort
																																		and: [ 
																																			self repositoryResolutionPolicy = anObject repositoryResolutionPolicy
																																				and: [ 
																																					self _platformProperties = anObject _platformProperties
																																						or: [ self platformProperties = anObject platformProperties ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
]

{ #category : 'converting' }
RwProjectSpecificationV2 >> asProjectDefiniton [
	^ RwProjectDefinitionV2 fromLoadSpecification: self
]

{ #category : 'converting' }
RwProjectSpecificationV2 >> asProjectDefinitonWithComment: commentString [
	^ (RwProjectDefinitionV2 fromLoadSpecification: self)
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> componentsPath [

	^ componentsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> componentsPath: aString [
	componentsPath := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> componentsRoot [
	^ self repositoryRoot / self componentsPath
]

{ #category : 'private' }
RwProjectSpecificationV2 >> currentVersion [
	"
		0.1.0 - first class load specification
	"
	^ '0.1.0'
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> diskUrl [
	^ diskUrl
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> diskUrl: anUrlString [
	diskUrl := anUrlString
]

{ #category : 'actions' }
RwProjectSpecificationV2 >> export [
	^ self exportTo: self specsRoot
]

{ #category : 'actions' }
RwProjectSpecificationV2 >> exportTo: directoryReference [
	self exportTo: directoryReference as: self specName , '.ston'
]

{ #category : 'actions' }
RwProjectSpecificationV2 >> exportTo: directoryReference as: fileName [
	directoryReference / fileName
		writeStreamDo: [ :fileStream | STON put: self copy initializeForExport onStreamPretty: fileStream ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> gitUrl [
	^ gitUrl
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> gitUrl: anUrlString [
	gitUrl := anUrlString
]

{ #category : 'comparing' }
RwProjectSpecificationV2 >> hash [
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: self projectAlias hash.
	hashValue := hashValue bitXor: self componentsPath hash.
	hashValue := hashValue bitXor: self packagesPath hash.
	hashValue := hashValue bitXor: self projectsPath hash.
	hashValue := hashValue bitXor: self specsPath hash.
	hashValue := hashValue bitXor: self projectsHome hash.
	hashValue := hashValue bitXor: self _repoType hash.
	hashValue := hashValue bitXor: self gitUrl hash.
	hashValue := hashValue bitXor: self diskUrl hash.
	hashValue := hashValue bitXor: self mercurialUrl hash.
	hashValue := hashValue bitXor: self svnUrl hash.
	hashValue := hashValue bitXor: self revision hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self componentNames hash.
	hashValue := hashValue bitXor: self groupNames hash.
	hashValue := hashValue bitXor: self repositoryResolutionPolicy hash.
	hashValue := hashValue bitXor: self _platformProperties hash.
	^ hashValue
]

{ #category : 'other' }
RwProjectSpecificationV2 >> initialize [
	super initialize.
	componentsPath := 'rowan/components'.
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'
]

{ #category : 'initialization' }
RwProjectSpecificationV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	projectsHome := repoType := repositoryDefinition := nil
]

{ #category : 'testing' }
RwProjectSpecificationV2 >> isStrict [
	^ self repositoryResolutionPolicy == #strict
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> mercurialUrl [
	^ mercurialUrl
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> mercurialUrl: anUrlString [
	mercurialUrl := anUrlString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageConvention [
	^ packageConvention ifNil: [ 'RowanHybrid' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageConvention: aString [
	packageConvention := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageFormat [
	^ self packageFormatIfAbsent: [ 'tonel' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageFormat: aString [
	(#('tonel' 'filetree') includes: aString)
		ifFalse: [ 
			self
				error:
					'Unknown package format ' , aString printString
						, '. Should be one of: tonel, or filetree' ].
	packageFormat := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageFormatIfAbsent: absentBlock [
	^ packageFormat ifNil: absentBlock
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packagesPath [

	^ packagesPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packagesPath: aString [
	packagesPath := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packagesRoot [
	^ self repositoryRoot / self packagesPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> platformProperties [
	^ platformProperties ifNil: [ platformProperties := Dictionary new ]
]

{ #category : 'copying' }
RwProjectSpecificationV2 >> postCopy [
	"don't forget to copy any compound state objects"

	super postCopy.
	repositoryDefinition := repositoryDefinition copy.
	platformProperties
		ifNotNil: [ 
			| platformPropertiesCopy |
			platformPropertiesCopy := platformProperties copy.
			platformProperties
				keysAndValuesDo: [ :platformName :userIdMap | 
					| userIdMapCopy |
					userIdMapCopy := userIdMap copy.
					platformPropertiesCopy at: platformName put: userIdMapCopy.
					platformName = 'gemstone'
						ifTrue: [ 
							userIdMap
								keysAndValuesDo: [ :userId :platformPropertiesMap | 
									| platformPropertiesMapCopy |
									platformPropertiesMapCopy := platformPropertiesMap copy.
									userIdMapCopy at: userId put: platformPropertiesMapCopy.
									platformPropertiesMap
										keysAndValuesDo: [ :propertyKey :propertyValue | platformPropertiesMapCopy at: propertyKey put: propertyValue copy ] ] ]
						ifFalse: [ self error: 'postCopy of ' , platformName printString , ' not yet implemented' ] ].
			platformProperties := platformPropertiesCopy ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectName [

	^ projectName
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectRoots [
	^ {(self componentsRoot).
	(self packagesRoot).
	(self specsRoot).
	(self projectsRoot)}
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsHome [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	^ projectsHome ifNil: [FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsHome: aStringOrFileReference [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	projectsHome := aStringOrFileReference asFileReference
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsPath [

	^ projectsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsPath: aString [
	projectsPath := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsRoot [
	^ self repositoryRoot / self projectsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectUrl [
	gitUrl ifNotNil: [ :urlString | ^ urlString ].
	diskUrl ifNotNil: [ :urlString | ^ urlString ].
	mercurialUrl ifNotNil: [ :urlString | ^ urlString ].
	svnUrl ifNotNil: [ :urlString | ^ urlString ].
	^ self error: 'internal error - no projectUrl has been defined'
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repositoryDefinition [
	^ repositoryDefinition
		ifNil: [ 
			repositoryDefinition := self gitUrl
				ifNotNil: [ :urlString | 
					RwGitRepositoryDefinitionV2
						newNamed: self projectAlias
						projectsHome: self projectsHome
						repositoryUrl: urlString
						revision: self revision ]
				ifNil: [ 
					self svnUrl
						ifNotNil: [ :urlString | Error signal: 'Svn repositories not supported, yet' ]
						ifNil: [ 
							self mercurialUrl
								ifNotNil: [ :urlString | Error signal: 'Mercurial repositories not supported, yet' ]
								ifNil: [ 
									self repoType == #'git'
										ifTrue: [ 
											RwGitRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: ''
												revision: self revision ]
										ifFalse: [ 
											RwDiskRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: self diskUrl ] ] ] ].

			repositoryDefinition ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repositoryDefinition: aRwRepositoryDefinition [
	repositoryDefinition := aRwRepositoryDefinition
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repositoryResolutionPolicy [
	^repositoryResolutionPolicy
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repositoryResolutionPolicy: aSymbolOrNil [
	"
	#strict - repository will be forced to match the specificed revision
	"

	repositoryResolutionPolicy := aSymbolOrNil
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repositoryRoot [
	repositoryDefinition ifNil: [ ^ self projectsHome / self projectAlias ].
	^ self repositoryDefinition repositoryRoot
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repoType [
	^ repoType ifNil: [ repoType := #disk ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repoType: aSymbol [
	(#(#'disk' #'git') includes: aSymbol asSymbol)
		ifFalse: [ self error: 'Unknown repo type ' , aSymbol asSymbol printString ].
	^ repoType := aSymbol asSymbol
]

{ #category : 'actions' }
RwProjectSpecificationV2 >> resolve [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 projectSpecification: self
]

{ #category : 'actions' }
RwProjectSpecificationV2 >> resolveStrict [
	"resolve using #strict repositoryResolutionpolicy"

	| oldPolicy |
	self isStrict
		ifTrue: [ ^ self resolve ].
	oldPolicy := self repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checked out out in the repository"
	self repositoryResolutionPolicy: #'strict'.
	^ self resolve ]
		ensure: [ self repositoryResolutionPolicy: oldPolicy ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> revision [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> revision: aRevisionString [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision := aRevisionString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specName [

	^ specName ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specsPath [

	^ specsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specsPath: aString [
	specsPath := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specsRoot [
	^ self repositoryRoot / self specsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> svnUrl [
	^ svnUrl
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> svnUrlUrl: anUrlString [
	svnUrl := anUrlString
]
