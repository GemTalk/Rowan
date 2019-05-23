private
_rowanSample2_gitoliteSpecificationUrl

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath
		, '/test/specs/RowanSample2_gitolite.ston'
