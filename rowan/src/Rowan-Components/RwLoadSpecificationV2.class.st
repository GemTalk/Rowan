Class {
	#name : 'RwLoadSpecificationV2',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectName',
		'projectAlias',
		'gitUrl',
		'diskUrl',
		'mercurialUrl',
		'svnUrl',
		'revision',
		'projectSpecFile',
		'componentNames',
		'groupNames',
		'platformProperties',
		'comment',
		'projectsHome',
		'repositoryResolutionPolicy'
	],
	#category : 'Rowan-Components'
}

{ #category : 'accessing' }
RwLoadSpecificationV2 >> _platformProperties [
	"direct access to IV ... used by ="

	^ platformProperties
]

{ #category : 'private' }
RwLoadSpecificationV2 >> _validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| repoUrls |
	#(#'componentNames' #'groupNames' #'specName' #'projectSpecFile')
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
					self revision
						ifNil: [ 
							Error
								signal:
									'The instance variable ''revision'' must be set for the'
										, (repoUrls at: 1) asString printString ] ] ].
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
RwLoadSpecificationV2 >> _validateGemStonePlatformUserIdMap: userIdMap [
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
RwLoadSpecificationV2 >> = anObject [
	^ self specName = anObject specName
		and: [ 
			self projectName = anObject projectName
				and: [ 
					self projectAlias = anObject projectAlias
						and: [ 
							self projectsHome = anObject projectsHome
								and: [ 
									self componentNames asArray sort = anObject componentNames asArray sort
										and: [ 
											self groupNames asArray sort = anObject groupNames asArray sort
												and: [ 
													self projectSpecFile = anObject projectSpecFile
														and: [ 
															self repositoryResolutionPolicy = anObject repositoryResolutionPolicy
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
																													self _platformProperties = anObject _platformProperties
																														or: [ self platformProperties = anObject platformProperties ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> componentNames [
	^ componentNames
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> componentNames: anArray [
	componentNames := anArray asArray
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> diskUrl [
	^ diskUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> diskUrl: anUrlString [
	diskUrl := anUrlString
]

{ #category : 'exporting' }
RwLoadSpecificationV2 >> exportTo: directoryReference [
	(directoryReference / self specName, 'ston')
		writeStreamDo: [ :fileStream | STON put: self copy initializeForExport onStreamPretty: fileStream ]
]

{ #category : 'ston' }
RwLoadSpecificationV2 >> fromSton: stonReader [
	"return an instance of RwResolvedLoadSpecificationV2 that wraps the receiver"

	^ (super fromSton: stonReader)
		_validate;
		yourself
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> gitUrl [
	^ gitUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> gitUrl: anUrlString [
	gitUrl := anUrlString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> groupNames [
	^ groupNames
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> groupNames: anArray [
	groupNames := anArray asArray
]

{ #category : 'comparing' }
RwLoadSpecificationV2 >> hash [
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: self projectAlias hash.
	hashValue := hashValue bitXor: self projectSpecFile hash.
	hashValue := hashValue bitXor: self gitUrl hash.
	hashValue := hashValue bitXor: self diskUrl hash.
	hashValue := hashValue bitXor: self mercurialUrl hash.
	hashValue := hashValue bitXor: self svnUrl hash.
	hashValue := hashValue bitXor: self revision hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self componentNames hash.
	hashValue := hashValue bitXor: self projectsHome hash.
	hashValue := hashValue bitXor: self groupNames hash.
	hashValue := hashValue bitXor: self repositoryResolutionPolicy hash.
	hashValue := hashValue bitXor: self _platformProperties hash.
	^ hashValue
]

{ #category : 'initialization' }
RwLoadSpecificationV2 >> initialize [
	super initialize.
	componentNames := {}.
	groupNames := {'core'}.
	projectSpecFile := 'rowan/project.ston'.
	comment := ''
]

{ #category : 'initialization' }
RwLoadSpecificationV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	repositoryResolutionPolicy := projectsHome :=  nil
]

{ #category : 'testing' }
RwLoadSpecificationV2 >> isStrict [
	^ self repositoryResolutionPolicy == #strict
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> mercurialUrl [
	^ mercurialUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> mercurialUrl: anUrlString [
	mercurialUrl := anUrlString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> platformProperties [
	^ platformProperties ifNil: [ platformProperties := Dictionary new ]
]

{ #category : 'copying' }
RwLoadSpecificationV2 >> postCopy [
	"don't forget to copy any compound state objects"

	super postCopy.
	componentNames := componentNames copy.
	groupNames := groupNames copy.
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
RwLoadSpecificationV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectAlias: aString [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	projectAlias := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectName [
	^ projectName
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectsHome [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	^ projectsHome ifNil: [FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectsHome: aStringOrFileReference [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	projectsHome := aStringOrFileReference asFileReference
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectSpecFile [
	^ projectSpecFile
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectSpecFile: aString [
	projectSpecFile := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> repositoryResolutionPolicy [
	^repositoryResolutionPolicy
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> repositoryResolutionPolicy: aSymbolOrNil [
	"
	#strict - repository will be forced to match the specificed revision
	"

	repositoryResolutionPolicy := aSymbolOrNil
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> repositoryRoot [
	^ self projectsHome / self projectAlias
]

{ #category : 'actions' }
RwLoadSpecificationV2 >> resolve [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecification: self
]

{ #category : 'actions' }
RwLoadSpecificationV2 >> resolveStrict [
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
RwLoadSpecificationV2 >> revision [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> revision: aRevisionString [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision := aRevisionString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> specName [

	^ specName ifNil: [ self projectAlias ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> svnUrl [
	^ svnUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> svnUrlUrl: anUrlString [
	svnUrl := anUrlString
]
