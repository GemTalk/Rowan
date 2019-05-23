instance creation
newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString

	^ self 
		newNamed: aRwSpecification specName 
		forSpecification: aRwSpecification 
		projectHome: projectHomeFileReferenceOrString