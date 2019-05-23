smalltalk api
exportSpecification: aRwSpecification toUrl: fileUrl

	self specification: aRwSpecification.
	^specification exportToUrl: fileUrl
