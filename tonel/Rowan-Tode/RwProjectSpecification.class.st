Class {
	#name : 'RwProjectSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectUrl',
		'platformSpec',
		'specsPath',
		'repoSpec',
		'repoPath',
		'comment',
		'imageSpec',
		'packageInfoSource'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'instance creation' }
RwProjectSpecification class >> newNamed: aString [

	^ self new
		specName: aString;
		yourself
]

{ #category : 'private' }
RwProjectSpecification >> _myUserProfile [
  ^ System myUserProfile
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

{ #category : 'exporting' }
RwProjectSpecification >> export [
  | url exportUrl exportSpec |
  self repositoryUrl
    ifNil: [ self error: 'exportToUrl: must be used if repositoryUrl has not been set' ].
  url := RwUrl fromString: self repositoryUrl.
  exportUrl := 'file:'
    ,
      (ServerFileDirectory on: url pathForDirectory) containingDirectory fullName
    , '/' , self specsPath , '/'.
  exportSpec := self copy.
  exportSpec initializeForExport.	"clear fields that only apply to local image"
  ^ exportSpec exportToUrl: exportUrl
]

{ #category : 'exporting' }
RwProjectSpecification >> exportToUrl: fileUrl [
  | url |
  url := CypUrl fromString: fileUrl.
  url schemeName = 'file'
    ifTrue: [ 
      CypressFileUtilities current
        writeStreamFor: self specName , '.ston'
        in: url pathForDirectory
        do: [ :stream | 
          | string |
          string := STON toStringPretty: self.
          stream nextPutAll: string.
          ^ self ] ].
  ^ nil	"otherwise a noop"
]

{ #category : 'accessing' }
RwProjectSpecification >> imageSpec [
  self subclassResponsibility: #'imageSpec'
]

{ #category : 'accessing' }
RwProjectSpecification >> imageSpec: anObject [

   imageSpec := anObject

]

{ #category : 'initialization' }
RwProjectSpecification >> initialize [

	super initialize.
	comment := ''.
	specsPath := 'specs'.
	platformSpec := Dictionary new
		at: 'gemstone' put: RwGemStoneSpecification new;
		yourself.
	repoSpec := RwGitRepositorySpecification new.
	packageInfoSource := #'SymbolDictionary'
]

{ #category : 'initialization' }
RwProjectSpecification >> initializeForImport [
  "if spec has been imorted, clear out any of the fields that represent local disk state"

  super initializeForImport.
  projectUrl := imageSpec := nil.
  repoSpec initializeForImport
]

{ #category : 'accessing' }
RwProjectSpecification >> packageInfoSource [

	^ packageInfoSource
]

{ #category : 'accessing' }
RwProjectSpecification >> packageInfoSource: anObject [

   packageInfoSource := anObject

]

{ #category : 'accessing' }
RwProjectSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

	self subclassResponsibility: #packageOrGroupNamesToLoad:
]

{ #category : 'accessing' }
RwProjectSpecification >> platformSpec [

   ^platformSpec

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
RwProjectSpecification >> repoPath [

   ^repoPath

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

   ^specsPath

]

{ #category : 'accessing' }
RwProjectSpecification >> specsPath: anObject [

   specsPath := anObject

]

{ #category : 'actions' }
RwProjectSpecification >> updateLoadedCommitIdForTool: aRwTool [
  ^ self repoSpec updateLoadedCommitIdForTool: aRwTool
]

{ #category : 'actions' }
RwProjectSpecification >> updateLoadedPackageNamesTool: aRwTool packageNames: aPackageNames [
  ^ self imageSpec
    updateLoadedPackageNamesTool: aRwTool
    packageNames: aPackageNames
]
