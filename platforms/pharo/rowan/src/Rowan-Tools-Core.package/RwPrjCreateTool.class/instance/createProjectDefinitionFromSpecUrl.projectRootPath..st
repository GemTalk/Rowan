project definition creation
createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath
	"Answer a project definition based on the spec given in <fileUrlOrString> 
		<projectRootPath> is the path to the root directory of the Rowan project"

	| path fileUrl spec projectDefinition projectRootRef |
	fileUrl := fileUrlOrString asRwUrl.
	fileUrl scheme = 'file'
		ifFalse: [ self error: 'the url ', fileUrlOrString printString, ' is expected to be a file: url referencing a project specification file in an existing Rowan project' ].
	spec := fileUrl asSpecification.
	projectRootRef := projectRootPath  asFileReference.
	spec 
		repositoryRootPath: projectRootRef ;
		repositoryUrl: 'cypress:', (projectRootRef  / spec repoPath) pathString, '/'.
	path :=  spec repositoryRootPath / spec repoPath.
	path exists
		ifFalse: [ self error: 'No source repository found at ' , path pathString printString ].
	path :=  spec repositoryRootPath / spec configsPath.
	path exists
		ifFalse: [ self error: 'No configuration directory found at ' , path pathString printString ].
	projectDefinition := spec asDefinition.
	^ projectDefinition