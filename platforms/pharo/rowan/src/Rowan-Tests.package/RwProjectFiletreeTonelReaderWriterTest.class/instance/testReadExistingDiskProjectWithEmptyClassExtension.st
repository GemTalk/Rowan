tests
testReadExistingDiskProjectWithEmptyClassExtension
	"https://github.com/GemTalk/Rowan/issues/361"


	| rowanSpec projectHome projectName specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	projectName := 'Issue361'.
	specUrlString :=  'file:' , projectHome, '/', projectName, '/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition.

"validate"
	self _validateIssue361ProjectDefinitionSet: projectDefinitionSet projectName: projectName