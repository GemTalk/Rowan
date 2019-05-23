private
_rowanSample2_gitlabSpecificationUrl

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath
		, '/test/specs/RowanSample2_gitlab.ston'
