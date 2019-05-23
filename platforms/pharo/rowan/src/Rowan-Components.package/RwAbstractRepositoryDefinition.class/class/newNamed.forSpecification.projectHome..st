instance creation
newNamed: repositoryName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString

	| cls projectHomeFileReference |
	projectHomeFileReference := projectHomeFileReferenceOrString asFileReference.
	cls := (aRwSpecification useGit and: [ projectHomeFileReference fileSystem isDiskFileSystem ])
		ifTrue: [ RwGitRepositoryDefinition ]
		ifFalse: [  projectHomeFileReference fileSystem rowanRepositoryDefinitionClass ].
	^ (self _instanceClass: cls)
		newNamed: aRwSpecification specName 
			forSpecification: aRwSpecification 
			repositoryRoot: projectHomeFileReference / aRwSpecification specName