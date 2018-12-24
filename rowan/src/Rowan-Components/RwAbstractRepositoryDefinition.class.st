Class {
	#name : 'RwAbstractRepositoryDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

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

	^ self properties at: 'configsPath'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> configsPath: pathString [

	self properties at: 'configsPath' put: pathString
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
RwAbstractRepositoryDefinition >> repositoryRoot: pathString [

	self properties at: 'repositoryRoot' put: self _fileSystem / pathString
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> specsPath [

	^ self properties at: 'specsPath'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> specsPath: pathString [

	self properties at: 'specsPath' put: pathString
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> srcPath [

	^ self properties at: 'srcPath'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> srcPath: pathString [

	self properties at: 'srcPath' put: pathString
]
