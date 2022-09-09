Class {
	#name : 'RwProjectSpecificationV3',
	#superclass : 'RwProjectSpecificationV2',
	#instVars : [
		'projectVersion'
	],
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'comparing' }
RwProjectSpecificationV3 >> = anObject [
	^ super = anObject and: [ self projectVersion = anObject projectVersion ]
]

{ #category : 'comparing' }
RwProjectSpecificationV3 >> hash [
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: projectVersion hash.
	hashValue := hashValue bitXor: self componentsPath hash.
	hashValue := hashValue bitXor: self packagesPath hash.
	hashValue := hashValue bitXor: self projectsPath hash.
	hashValue := hashValue bitXor: self specsPath hash.
	hashValue := hashValue bitXor: self _repoType hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self packageFormat hash.
	hashValue := hashValue bitXor: self packageConvention hash.
	hashValue := hashValue bitXor: self loadedCommitId hash.
	^ hashValue
]

{ #category : 'initialization' }
RwProjectSpecificationV3 >> initialize [
	"for v3, packageConvention and packageForm is explicit and the default location of the packages directory is the root of the project tree ... keep the Rowan meta data isolated in the rowan directory"

	super initialize.
	packagesPath := 'src'.
	projectVersion := '0.0.0'.
	packageConvention := 'RowanHybrid'.
	packageFormat := 'tonel'
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
RwProjectSpecificationV3 >> projectVersion: aStringOrVersionOrNil [
	aStringOrVersionOrNil
		ifNil: [ projectVersion := nil ]
		ifNotNil: [ 
			aStringOrVersionOrNil asRwSemanticVersionNumber.	"expect an error if aStringOrVersion is not a valid semantic version number"
			projectVersion := aStringOrVersionOrNil asString ]
]
