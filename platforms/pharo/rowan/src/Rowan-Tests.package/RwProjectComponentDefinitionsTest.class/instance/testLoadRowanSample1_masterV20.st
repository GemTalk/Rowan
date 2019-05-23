tests
testLoadRowanSample1_masterV20

	| rowanSpec projectsHome urlString projectSample1Definition projectSample2Definition 
		projectSetDefinition1 projectSetDefinition2 x |

	#( 'RowanSample1' 'RowanSample2') do: [:prjName |
		(Rowan image loadedProjectNamed: prjName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectsHome := (rowanSpec repositoryRootPath , '/test/testRepositories/repos') asFileReference.

	urlString :=  'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample1_masterV2.ston'.
	projectSample1Definition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: projectsHome;
		yourself.

	urlString :=  'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample2_masterV2.ston'.
	projectSample2Definition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: projectsHome;
		yourself.

"setup"
	projectSample1Definition repositoryRoot ensureDeleteAll.
	projectSample2Definition repositoryRoot ensureDeleteAll.

"clone"
	projectSetDefinition2 := projectSample2Definition clone.	"we want RowanSample2 to be freshly cloned into projectsHome"
	projectSetDefinition1 := projectSample1Definition clone.

"validate"
	self assert: (x := projectSetDefinition1 projectNames asArray sort) = #( 'RowanSample1' 'RowanSample2').

"load both 1 and 2"
	projectSample1Definition load.

"validate"