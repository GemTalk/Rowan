project definition creation
createProjectDefinitionFromSpecUrl: fileUrlOrString
	"Answer a project definition based on the given <fileUrlOrString> 
		<fileUrlOrString> is expected to be an RwFileUrl for a project spec _in_ an existing Rowan project"

	| fileUrl spec specsPath specFilePath index projectRootPath |
	fileUrl := fileUrlOrString asRwUrl.
	fileUrl scheme = 'file'
		ifFalse: [ self error: 'the url ', fileUrlOrString printString, ' is expected to be a file: url referencing a project specification file in an existing Rowan project' ].
	spec := fileUrl asSpecification.
	specsPath := spec specsPath.
    specFilePath := fileUrl pathDirString.
	index := specFilePath findString: specsPath startingAt: 1.
	projectRootPath := specFilePath copyFrom: 1 to: index - 1.
	^ self createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath
