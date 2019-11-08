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
		'projectUrl',
		'commitish',
		'componentsPath',
		'specsPath',
		'packagesPath',
		'comment',
		'componentNames',
		'groupNames',
		'platformProperties'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectLoadSpecificationV2 class >> specName: specName projectName: projectName comment: commentString [
	^ self new
		specName: projectName;
		projectName: projectName;
		comment: commentString;
		yourself
]

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
RwProjectLoadSpecificationV2 >> commitish [
	^ commitish
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> commitish: aString [
	commitish := aString
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
	commitish := 'master'.
	componentNames := #('Core').
	componentsPath := 'rowan/components'.
	groupNames := #('core' 'tests').
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'.
	projectName := 'ProjectName'
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
RwProjectLoadSpecificationV2 >> projectUrl [
	^ projectUrl ifNil: [ '' ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectUrl: aString [
	projectUrl := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specName [

	^ specName
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

{ #category : 'private' }
RwProjectLoadSpecificationV2 >> validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	#(#'commitish' #'componentNames' #'componentsPath' #'groupNames' #'packagesPath' #'projectsPath' #'specsPath' #'projectName' #'specName')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	self platformProperties
		keysAndValuesDo: [ :platformName :userIdMap | 
			(RwAbstractProjectLoadComponentV2 _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString
								, ' in platform properties' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatformUserIdMap: userIdMap ] ].
	^ true
]
