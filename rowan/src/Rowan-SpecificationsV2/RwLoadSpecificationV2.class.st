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
		'readonlyDiskUrl',
		'relativeRepositoryRoot',
		'svnUrl',
		'revision',
		'projectSpecFile',
		'componentNames',
		'groupNames',
		'customConditionalAttributes',
		'platformProperties',
		'comment',
		'projectsHome',
		'repositoryResolutionPolicy'
	],
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 class >> _gemstoneAllUsersName [
	^ 'allusers'
]

{ #category : 'accessing' }
RwLoadSpecificationV2 class >> label [
	^ 'load specification '
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> _gemstoneAllUsersName [

	^ self class _gemstoneAllUsersName
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> _gemstoneDefaultMethodEnv [
	^ 0
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> _gemstoneDefaultSymbolDictName [

	^ 'UserGlobals'
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> _gemstoneDefaultUseSessionMethodsForExtensions [

	^ false
]

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
	#(#'componentNames' #'specName' #'projectSpecFile')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	repoUrls := {}.
	#(#'gitUrl' #'diskUrl' #'mercurialUrl' #'svnUrl' #readonlyDiskUrl)
		do: [ :messageName | (self perform: messageName) ifNotNil: [ repoUrls add: messageName asString ] ].
	repoUrls size > 1
		ifTrue: [ Error signal: 'Only one of (gitUrl diskUrl mercurialUrl readonlyDiskUrl svnUrl) must be be set' ].
	(repoUrls size = 0 or: [ (repoUrls includes: 'diskUrl') or: [repoUrls includes: 'readonlyDiskUrl']])
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
			(self class _supportedPlatformNames includes: platformName)
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
									self customConditionalAttributes asArray sort
										= anObject customConditionalAttributes asArray sort
										and: [ 
											self componentNames asArray sort = anObject componentNames asArray sort
												and: [ 
													self projectSpecFile = anObject projectSpecFile
														and: [ 
															self repositoryResolutionPolicy = anObject repositoryResolutionPolicy
																and: [ 
																	self gitUrl = anObject gitUrl
																		and: [ 
																			self diskUrl = anObject diskUrl
																				and: [ 
																			self readonlyDiskUrl = anObject readonlyDiskUrl
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
																														or: [ self platformProperties = anObject platformProperties ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
]

{ #category : 'visiting' }
RwLoadSpecificationV2 >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadSpecification: self
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> addTopLevelComponentNamed: componentName [
	componentNames add: componentName
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
RwLoadSpecificationV2 >> customConditionalAttributes [

	^ customConditionalAttributes ifNil: [ #() ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> customConditionalAttributes: anArray [

	customConditionalAttributes := anArray
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> diskUrl [
	^ diskUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> diskUrl: anUrlString [
	revision := gitUrl := diskUrl := mercurialUrl := readonlyDiskUrl := svnUrl := nil.
	diskUrl := anUrlString
]

{ #category : 'exporting' }
RwLoadSpecificationV2 >> exportTo: directoryReference [
	directoryReference / self specName , 'ston'
		writeStreamDo:
			[ :fileStream |
			fileStream truncate.
			self exportToStream: fileStream ]
]

{ #category : 'exporting' }
RwLoadSpecificationV2 >> exportToStream: fileStream [
	STON put: self copy initializeForExport onStreamPretty: fileStream
]

{ #category : 'ston' }
RwLoadSpecificationV2 >> fromSton: stonReader [
	"return an instance of RwResolvedLoadSpecificationV2 that wraps the receiver"

	^ (super fromSton: stonReader)
		_validate;
		yourself
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneDefaultMethodEnvForUser: userId [
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultMethodEnv ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultMethodEnv ] ].
	^ userProperties
		at: #'defaultMethodEnv'
		ifAbsent: [ self _gemstoneDefaultMethodEnv ]
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneDefaultSymbolDictName [
	^ self gemstoneDefaultSymbolDictNameForUser: self _gemstoneAllUsersName
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultSymbolDictName ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultSymbolDictName ] ].
	^ (userProperties
		at: #'defaultSymbolDictName'
		ifAbsent: [ self _gemstoneDefaultSymbolDictName ]) asString
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId [
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultUseSessionMethodsForExtensions ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultUseSessionMethodsForExtensions ] ].
	^ userProperties
		at: #'defaultUseSessionMethodsForExtensions'
		ifAbsent: [ self _gemstoneDefaultUseSessionMethodsForExtensions ]
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneSetDefaultMethodEnvForUser: userId to: env [

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultMethodEnv put: env
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneSetDefaultMethodEnvTo: env [
	self gemstoneSetDefaultMethodEnvForUser: self _gemstoneAllUsersName to: env
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultSymbolDictName put: symbolDictName asString
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self gemstoneSetDefaultSymbolDictNameForUser: self _gemstoneAllUsersName to: symbolDictName
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool [
	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId
		ifAbsentPut: [ Dictionary new ])
		at: #'defaultUseSessionMethodsForExtensions'
		put: aBool
]

{ #category : 'gemstone-support' }
RwLoadSpecificationV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool [
	self gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: self _gemstoneAllUsersName to: aBool
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> gitUrl [
	^ gitUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> gitUrl: anUrlString [
	gitUrl := diskUrl := mercurialUrl := readonlyDiskUrl := svnUrl := nil.
	gitUrl := anUrlString
]

{ #category : 'comparing' }
RwLoadSpecificationV2 >> hash [
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self customConditionalAttributes hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: self projectAlias hash.
	hashValue := hashValue bitXor: self projectSpecFile hash.
	hashValue := hashValue bitXor: self gitUrl hash.
	hashValue := hashValue bitXor: self diskUrl hash.
	hashValue := hashValue bitXor: self readonlyDiskUrl hash.
	hashValue := hashValue bitXor: self mercurialUrl hash.
	hashValue := hashValue bitXor: self svnUrl hash.
	hashValue := hashValue bitXor: self revision hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self componentNames hash.
	hashValue := hashValue bitXor: self projectsHome hash.
	hashValue := hashValue bitXor: self repositoryResolutionPolicy hash.
	hashValue := hashValue bitXor: self _platformProperties hash.
	^ hashValue
]

{ #category : 'initialization' }
RwLoadSpecificationV2 >> initialize [
	super initialize.
	componentNames := {}.
	projectSpecFile := 'rowan/project.ston'.
	comment := ''
]

{ #category : 'initialization' }
RwLoadSpecificationV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	platformProperties
		ifNotNil: [ 
			platformProperties isEmpty
				ifTrue: [ platformProperties := nil ] ].
	repositoryResolutionPolicy := projectsHome := nil
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
	gitUrl := diskUrl := mercurialUrl := readonlyDiskUrl := svnUrl := nil.
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
	customConditionalAttributes := customConditionalAttributes copy.
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

{ #category : 'printing' }
RwLoadSpecificationV2 >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: (self specName ifNil: ['nil'])
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

	^ projectsHome ifNil: [FileLocator origin: #'ROWAN_PROJECTS_HOME']
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
RwLoadSpecificationV2 >> projectUrl [
	^ self gitUrl
		ifNotNil: [ :urlString | urlString ]
		ifNil: [ 
			self svnUrl
				ifNotNil: [ :urlString | urlString ]
				ifNil: [ self mercurialUrl ifNotNil: [ :urlString | urlString ] ifNil: [ self diskUrl ifNil: [ self readonlyDiskUrl ]] ] ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> readonlyDiskUrl [
	^ readonlyDiskUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> readonlyDiskUrl: anUrlString [
	revision := gitUrl := diskUrl := mercurialUrl := readonlyDiskUrl := svnUrl := nil.
	readonlyDiskUrl := anUrlString
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
RwLoadSpecificationV2 >> resolveProjectSetStrict [
	"resolve using #strict repositoryResolutionpolicy"

	| oldPolicy |
	self isStrict
		ifTrue: [ ^ self resolve ].
	oldPolicy := self repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checked out out in the repository"
	self repositoryResolutionPolicy: #'strict'.
	^ self resolveProjectSet ]
		ensure: [ self repositoryResolutionPolicy: oldPolicy ]
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

{ #category : 'actions' }
RwLoadSpecificationV2 >> resolveWithParentProject: aResolvedProject [
	"give embedded projects a chance to resolve cleanly"

	self projectsHome: aResolvedProject projectsHome.
	^self  resolve
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

	^ specName ifNil: [ self projectName ]
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
RwLoadSpecificationV2 >> svnUrl: anUrlString [
	gitUrl := diskUrl := mercurialUrl := readonlyDiskUrl := svnUrl := nil.
	svnUrl := anUrlString
]
