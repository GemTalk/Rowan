tests
testReadComponentProject

	| rowanSpec urlString projectDefinition x projectSetDefinition |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	urlString :=  'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample2_masterV2.ston'.

	projectDefinition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: (rowanSpec repositoryRootPath , '/test/testRepositories/repos') asFileReference;
		yourself.

"setup"
	projectDefinition repositoryRoot ensureDeleteAll.
	self assert: (x := projectDefinition packageNames isEmpty).

"clone"
	projectSetDefinition := projectDefinition clone.	"does a read as part of the clone"

"validate"
	self assert: (x := projectSetDefinition projectNames asArray sort) = #( 'RowanSample2').
	self assert: (x := projectDefinition packageNames asArray sort) = #( 'RowanSample2-Core')