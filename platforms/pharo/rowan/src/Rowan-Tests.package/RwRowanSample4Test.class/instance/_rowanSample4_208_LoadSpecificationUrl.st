private
_rowanSample4_208_LoadSpecificationUrl

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample4_208_load.ston'
