Class {
	#name : 'RwProjectSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'version',
		'projectUrl',
		'platformSpec',
		'configsPath',
		'specsPath',
		'repoSpec',
		'repoPath',
		'comment',
		'imageSpec',
		'defaultConfigurationNames',
		'defaultGroupNames'
	],
	#category : 'Rowan-Specifications'
}

{ #category : 'instance creation' }
RwProjectSpecification class >> newNamed: aString [

	^ self new
		specName: aString;
		yourself
]

{ #category : 'exporting' }
RwProjectSpecification >> _exportToUrl: fileUrl [
	^ self _exportToUrl: fileUrl as: self specName , '.ston'

]

{ #category : 'exporting' }
RwProjectSpecification >> _exportToUrl: fileUrl as: fileName [
  | url |
  url := RwUrl fromString: fileUrl.
  url schemeName = 'file'
    ifTrue: [ 
      CypressFileUtilities current
        writeStreamFor: fileName
        in: url pathForDirectory
        do: [ :stream | 
          | string |
          string := STON toStringPretty: self.
          stream nextPutAll: string.
          ^ self ] ].
  ^ nil	"otherwise a noop"

]

{ #category : 'private' }
RwProjectSpecification >> _myUserProfile [
  ^ System myUserProfile
]

{ #category : 'converting' }
RwProjectSpecification >> asDefinition [

	"answer an RwProjectDefintion created using the reciever"

	^RwProjectDefinition newForSpecification: self

]

{ #category : 'testing' }
RwProjectSpecification >> canCommit [
  ^ self repositoryUrl notNil and: [ self repoSpec canCommit ]
]

{ #category : 'accessing' }
RwProjectSpecification >> comment [

   ^comment

]

{ #category : 'accessing' }
RwProjectSpecification >> comment: anObject [

   comment := anObject

]

{ #category : 'actions' }
RwProjectSpecification >> commitForTool: aRwTool message: messageString [
  ^ self repoSpec commitForTool: aRwTool message: messageString
]

{ #category : 'actions' }
RwProjectSpecification >> commitLogForTool: aRwTool limit: logLimit [
  ^ self repoSpec commitLogForTool: aRwTool limit: logLimit
]

{ #category : 'accessing' }
RwProjectSpecification >> configsPath [

	^ configsPath ifNil: [ configsPath := 'rowan/configs' ]

]

{ #category : 'accessing' }
RwProjectSpecification >> configsPath: anObject [

   configsPath := anObject

]

{ #category : 'accessing' }
RwProjectSpecification >> defaultConfigurationNames [

	^ defaultConfigurationNames

]

{ #category : 'accessing' }
RwProjectSpecification >> defaultGroupNames [

	^ defaultGroupNames

]

{ #category : 'exporting' }
RwProjectSpecification >> export [

	| url exportUrl exportPath |
	self repositoryUrl
		ifNil: [ self error: 'exportToUrl: must be used if repositoryUrl has not been set' ].
	url := RwUrl fromString: self repositoryUrl.
	exportPath := self repositoryRootPath , '/' , self specsPath , '/'.
	CypressFileUtilities current ensureDirectoryExists: exportPath.
	exportUrl := 'file:' , exportPath.
	^ self exportToUrl: exportUrl

]

{ #category : 'exporting' }
RwProjectSpecification >> exportToUrl: fileUrl [

	^ self copy
		initializeForExport;
		_exportToUrl: fileUrl

]

{ #category : 'exporting' }
RwProjectSpecification >> exportToUrl: fileUrl as: fileName [

	^ self copy
		initializeForExport;
		_exportToUrl: fileUrl as: fileName

]

{ #category : 'accessing' }
RwProjectSpecification >> imageSpec [

	^ imageSpec ifNil: [ imageSpec := RwSimpleProjectImageSpecification new ]

]

{ #category : 'accessing' }
RwProjectSpecification >> imageSpec: anObject [

   imageSpec := anObject

]

{ #category : 'initialization' }
RwProjectSpecification >> initializeForExport [

	"before spec is exported, clear out any of the fields that are not to be included"

	super initializeForExport.
	imageSpec := nil.
	platformSpec valuesDo: [:each | each initializeForExport ].
	repoSpec initializeForExport

]

{ #category : 'initialization' }
RwProjectSpecification >> initializeForImport [

	"if spec has been imported, clear out any of the fields that represent local disk state"

	super initializeForImport.
	imageSpec := nil.
	platformSpec valuesDo: [:each | each initializeForImport ].
	repoSpec initializeForImport

]

{ #category : 'accessing' }
RwProjectSpecification >> loadedCommitId [

	imageSpec ifNil: [ ^ '' ].
	^ self imageSpec loadedCommitId

]

{ #category : 'accessing' }
RwProjectSpecification >> loadedConfigurationNames [

	imageSpec ifNil: [ ^ #() ].
	^ self imageSpec loadedConfigurationNames

]

{ #category : 'accessing' }
RwProjectSpecification >> loadedGroupNames [

	imageSpec ifNil: [ ^ #() ].
	^ self imageSpec loadedGroupNames

]

{ #category : 'accessing' }
RwProjectSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

	self subclassResponsibility: #packageOrGroupNamesToLoad:
]

{ #category : 'accessing' }
RwProjectSpecification >> platformSpec: anObject [

   platformSpec := anObject

]

{ #category : 'copying' }
RwProjectSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  | oldPlatformSpec |
  super postCopy.
  oldPlatformSpec := platformSpec.
  platformSpec := Dictionary new.
  oldPlatformSpec
    keysAndValuesDo: [ :key :value | platformSpec at: key put: value copy ].
  repoSpec := repoSpec copy.
  imageSpec := imageSpec copy
]

{ #category : 'accessing' }
RwProjectSpecification >> projectUrl [

   ^projectUrl

]

{ #category : 'accessing' }
RwProjectSpecification >> projectUrl: anObject [

   projectUrl := anObject

]

{ #category : 'actions' }
RwProjectSpecification >> pullForTool: aRwTool remote: remoteString [
  ^ self repoSpec pullForTool: aRwTool remote: remoteString
]

{ #category : 'actions' }
RwProjectSpecification >> pushForTool: aRwTool remote: remoteString [
  ^ self repoSpec pushForTool: aRwTool remote: remoteString
]

{ #category : 'accessing' }
RwProjectSpecification >> remote [

	^ self repoSpec remote
]

{ #category : 'accessing' }
RwProjectSpecification >> repoPath [

   ^repoPath ifNil: [ repoPath := 'rowan/src' ]

]

{ #category : 'accessing' }
RwProjectSpecification >> repoPath: anObject [

   repoPath := anObject

]

{ #category : 'accessing' }
RwProjectSpecification >> repositoryRootPath [
  ^ self repoSpec repositoryRootPath
]

{ #category : 'accessing' }
RwProjectSpecification >> repositoryRootPath: anObject [
  self repoSpec repositoryRootPath: anObject
]

{ #category : 'accessing' }
RwProjectSpecification >> repositoryUrl [

   ^self imageSpec repositoryUrl

]

{ #category : 'accessing' }
RwProjectSpecification >> repositoryUrl: anObject [

   self imageSpec repositoryUrl: anObject

]

{ #category : 'accessing' }
RwProjectSpecification >> repoSpec [

   ^repoSpec

]

{ #category : 'accessing' }
RwProjectSpecification >> repoSpec: anObject [

   repoSpec := anObject

]

{ #category : 'accessing' }
RwProjectSpecification >> specName [

   ^specName
]

{ #category : 'accessing' }
RwProjectSpecification >> specName: anObject [

   specName := anObject

]

{ #category : 'accessing' }
RwProjectSpecification >> specsPath [

	^ specsPath ifNil: [ specsPath := 'rowan/specs' ]

]

{ #category : 'accessing' }
RwProjectSpecification >> specsPath: anObject [

   specsPath := anObject

]

{ #category : 'actions' }
RwProjectSpecification >> updateLoadedCommitIdForTool: aRwTool [
  ^ self repoSpec updateLoadedCommitIdForTool: aRwTool
]

{ #category : 'accessing' }
RwProjectSpecification >> useGit [

	^ self repoSpec useGit
]

{ #category : 'accessing' }
RwProjectSpecification >> version [
	"answer the version of the class --- used to mark the version of the class for objects written to disk"

	"see currentVersion method for list of versions"

	"currently RwProjectSpecification is the only instance being versioned as it represents the version of the whole structure, 
		but I don't want to rule out the possiblity that other specs may need to be independently versioned."

	^ version

]
