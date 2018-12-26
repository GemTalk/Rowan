Class {
	#name : 'RwAbstractRepositoryDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	| projectHomeFileReference |
	projectHomeFileReference := projectHomeFileReferenceOrString asFileReference.
	^ projectHomeFileReference fileSystem rowanRepositoryDefinitionClass
		newNamed: aRwSpecification specName 
		repositoryRoot: projectHomeFileReference / aRwSpecification specName 
		specsPath: aRwSpecification specsPath
		configsPath: aRwSpecification configsPath
		srcPath: aRwSpecification repoPath
]

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot specsPath: specsPath configsPath: configsPath srcPath: srcPath [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		specsPath: specsPath;
		configsPath: configsPath;
		srcPath: srcPath;
		yourself
]

{ #category : 'private' }
RwAbstractRepositoryDefinition >> _fileSystem [

	self subclassResponsibility: #_fileSystem
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> configsPath [

	^ self repositoryRoot / (self properties at: 'configsPath')
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> configsPath: pathStringOrPath [

	self properties at: 'configsPath' put: pathStringOrPath
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> name [

	^ self key
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> name: aString [ 

	properties at: 'name' put: aString
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> repositoryRoot [

	^ self properties at: 'repositoryRoot'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> repositoryRoot: pathStringOrReference [

	self properties at: 'repositoryRoot' put: pathStringOrReference asFileReference
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> specsPath [

	^ self repositoryRoot / (self properties at: 'specsPath')
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> specsPath: pathStringOrPath [

	self properties at: 'specsPath' put: pathStringOrPath
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> srcPath [

	^ self repositoryRoot / (self properties at: 'srcPath')
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> srcPath: pathStringOrPath [

	self properties at: 'srcPath' put: pathStringOrPath
]
