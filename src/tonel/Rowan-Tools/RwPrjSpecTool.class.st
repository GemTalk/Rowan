Class {
	#name : 'RwPrjSpecTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportProjectDefinition: projectDefinition [

	self specification: projectDefinition specification.
	^ specification export
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecification: aRwSpecification [

	self specification: aRwSpecification.
	^specification export
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecification: aRwSpecification toUrl: fileUrl [

	self specification: aRwSpecification.
	^specification exportToUrl: fileUrl

]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecUrl: aString [

	^ self exportSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecUrl: aString toUrl: exportUrl [

	^ self exportSpecification: (RwSpecification fromUrl: aString) toUrl: exportUrl
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> newPackageSpecNamed: aString useGit: useGit [
  | spec |
  spec := RwPackageProjectSpecification new
    specName: aString;
    yourself.
  useGit
    ifTrue: [ ^ spec ].
  spec repoSpec: RwDiskRepositorySpecification new.
  ^ spec
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> newProjectSpecNamed: aString useGit: useGit [
  | spec |
  spec := RwMetacelloProjectSpecification new
    specName: aString;
    yourself.
  useGit
    ifTrue: [ ^ spec ].
  spec repoSpec: RwDiskRepositorySpecification new.
  ^ spec
]
