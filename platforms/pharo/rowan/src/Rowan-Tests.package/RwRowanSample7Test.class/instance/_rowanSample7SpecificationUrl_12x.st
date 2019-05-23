private
_rowanSample7SpecificationUrl_12x

	"1.2.x compat project spec"

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample7_12x.ston'