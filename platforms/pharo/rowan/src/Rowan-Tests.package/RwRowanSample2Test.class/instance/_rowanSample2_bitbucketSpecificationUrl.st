private
_rowanSample2_bitbucketSpecificationUrl

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath
		, '/test/specs/RowanSample2_bitbucket.ston'
