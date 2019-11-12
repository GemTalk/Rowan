"
I am used to specify all of the properties that are used to load a project from disk.
"
Class {
	#name : 'RwProjectLoadSpecificationV2',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectName',
		'projectAlias',
		'projectsPath',
		'revision',
		'gitUrl',
		'diskUrl',
		'mercurialUrl',
		'svnUrl',
		'componentsPath',
		'specsPath',
		'packagesPath',
		'comment',
		'componentNames',
		'groupNames',
		'platformProperties',
		'repositoryDefinition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwProjectLoadSpecificationV2 >> _validateGemStonePlatformUserIdMap: userIdMap [
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

{ #category : 'converting' }
RwProjectLoadSpecificationV2 >> asProjectDefiniton [
	^ RwProjectDefinitionV2 fromLoadSpecification: self
]

{ #category : 'converting' }
RwProjectLoadSpecificationV2 >> asProjectDefinitonWithComment: commentString [
	^ (RwProjectDefinitionV2 fromLoadSpecification: self)
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentNames [
	^ componentNames
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentNames: anArray [
	componentNames := anArray asArray
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentsPath [

	^ componentsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentsPath: aString [
	componentsPath := aString
]

{ #category : 'private' }
RwProjectLoadSpecificationV2 >> currentVersion [
	"
		0.1.0 - first class load specification
	"
	^ '0.1.0'
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> diskUrl [
	^ diskUrl
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> diskUrl: anUrlString [
	diskUrl := anUrlString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> gitUrl [
	^ gitUrl
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> gitUrl: anUrlString [
	gitUrl := anUrlString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> groupNames [
	^ groupNames
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> groupNames: anArray [
	groupNames := anArray asArray
]

{ #category : 'initialization' }
RwProjectLoadSpecificationV2 >> initialize [
	super initialize.
	componentNames := #('Core').
	componentsPath := 'rowan/components'.
	groupNames := #('core' 'tests').
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'.
	projectName := 'ProjectName'
]

{ #category : 'initialization' }
RwProjectLoadSpecificationV2 >> initializeForExport [
  "if spec has been exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	repositoryDefinition := nil
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> mercurialUrl [
	^ mercurialUrl
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> mercurialUrl: anUrlString [
	mercurialUrl := anUrlString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> packagesPath [

	^ packagesPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> packagesPath: aString [
	packagesPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> platformProperties [
	^ platformProperties ifNil: [ platformProperties := Dictionary new ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectAlias: aString [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	projectAlias := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectName [

	^ projectName
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectsPath [

	^ projectsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectsPath: aString [
	projectsPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> repositoryDefinition [
	^ repositoryDefinition
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> repositoryDefinition: aRwRepositoryDefinition [
	repositoryDefinition := aRwRepositoryDefinition
]

{ #category : 'actions' }
RwProjectLoadSpecificationV2 >> resolve [
	^ self resolve: FileLocator rowanProjectsHome
]

{ #category : 'actions' }
RwProjectLoadSpecificationV2 >> resolve: aProjectHomePath [
	repositoryDefinition := self gitUrl
		ifNotNil: [ :urlString | 
			RwGitRepositoryDefinitionV2
				newNamed: self projectAlias
				repositoryRoot: aProjectHomePath asFileReference / self projectAlias
				repositoryUrl: urlString
				committish: self revision ]
		ifNil: [ 
			repositoryDefinition := self svnUrl
				ifNotNil: [ :urlString | Error signal: 'Svn repositories not supported, yet' ]
				ifNil: [ 
					repositoryDefinition := self mercurialUrl
						ifNotNil: [ :urlString | Error signal: 'Mercurial repositories not supported, yet' ]
						ifNil: [ 
							RwDiskRepositoryDefinitionV2
								newNamed: self projectAlias
								repositoryRoot: aProjectHomePath asFileReference / self projectAlias
								repositoryUrl: self diskUrl ] ] ].
	repositoryDefinition resolve.
	^ RwProjectDefinitionV2 fromLoadSpecification: self
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> revision [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> revision: aRevisionString [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision := aRevisionString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specName [

	^ specName ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specsPath [

	^ specsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specsPath: aString [
	specsPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> svnUrl [
	^ svnUrl
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> svnUrlUrl: anUrlString [
	svnUrl := anUrlString
]

{ #category : 'private' }
RwProjectLoadSpecificationV2 >> validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| repoUrls |
	#(#'componentNames' #'componentsPath' #'groupNames' #'packagesPath' #'projectsPath' #'specsPath' #'projectName' #'specName')
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
			Error
				signal:
					'The instance variable ''revision'' must be set for the'
						, (repoUrls at: 1) asString printString ].
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
