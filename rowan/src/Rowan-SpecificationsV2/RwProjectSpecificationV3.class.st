Class {
	#name : 'RwProjectSpecificationV3',
	#superclass : 'RwProjectSpecificationV2',
	#instVars : [
		'projectVersion'
	],
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'initialization' }
RwProjectSpecificationV3 >> initialize [
	super initialize.
	projectVersion := '0.0.0'
]

{ #category : 'ston' }
RwProjectSpecificationV3 >> instVarNamesInOrderForSton [
	^ #(#'specName' #'projectName' #'projectVersion' #'projectSpecPath' #'componentsPath' #'packagesPath' #'projectsPath' #'specsPath' #'packageFormat' #'packageConvention' #'comment' #'repoType' #'loadedCommitId')
]

{ #category : 'accessing' }
RwProjectSpecificationV3 >> projectVersion [
	^ projectVersion
		ifNotNil: [ :str | str asRwSemanticVersionNumber ]
		ifNil: [ '0.0.0' asRwSemanticVersionNumber ]
]

{ #category : 'accessing' }
RwProjectSpecificationV3 >> projectVersion: aStringOrVersion [
	aStringOrVersion asRwSemanticVersionNumber.	"expect an error if aStringOrVersion is not a valid semantic version number"
	projectVersion := aStringOrVersion asString
]
