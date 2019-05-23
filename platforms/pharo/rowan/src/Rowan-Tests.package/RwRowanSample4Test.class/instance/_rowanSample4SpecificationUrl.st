private
_rowanSample4SpecificationUrl

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , (rowanSpec repositoryRootPath asFileReference / 'test/specs/RowanSample4.ston') pathString