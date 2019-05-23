tests
testCreateProjectReferenceFromUrl

	| specUrlString rowanSpec gitRootPath projectName projectSpec projectReferenceDefinition x y |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.

"create project reference definitions"
	projectReferenceDefinition := RwProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: gitRootPath.

"validate"
	self assert: projectReferenceDefinition projectName = projectName.
	self assert: projectReferenceDefinition projectAlias = projectReferenceDefinition projectName.
	self assert: projectReferenceDefinition configurationNames = #( 'Main').
	self assert: projectReferenceDefinition groupNames = #( 'core').
	self assert: projectReferenceDefinition defaultComponentName = 'Main'.
	self assert: projectReferenceDefinition projectUrl = 'https://github.com/dalehenrich/RowanSample7'.

	self assert: (x := projectReferenceDefinition projectHome pathString) = (y := gitRootPath asFileReference pathString).

"create project reference definitions"
	projectReferenceDefinition := RwProjectReferenceDefinition newForSpecification: projectSpec.

"validate"
	self assert: (x := projectReferenceDefinition projectHome pathString) = (y := FileLocator rowanProjectsHome pathString).