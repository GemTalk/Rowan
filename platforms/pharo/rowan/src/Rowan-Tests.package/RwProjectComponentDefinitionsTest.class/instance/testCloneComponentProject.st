tests
testCloneComponentProject

	| rowanSpec urlString projectDefinition |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	urlString :=  'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample1_masterV2.ston'.

	projectDefinition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: (rowanSpec repositoryRootPath , '/test/testRepositories/repos') asFileReference;
		yourself.

"setup"
	projectDefinition repositoryRoot ensureDeleteAll.
	self deny: (projectDefinition repositoryRoot / 'rowan' / 'projects' / 'RowanSample2.ston') exists.

"clone"
	projectDefinition clone.

"validate"
	self assert: (projectDefinition repositoryRoot / 'rowan' / 'projects' / 'RowanSample2.ston') exists.	"present in masterV2.0 branch"