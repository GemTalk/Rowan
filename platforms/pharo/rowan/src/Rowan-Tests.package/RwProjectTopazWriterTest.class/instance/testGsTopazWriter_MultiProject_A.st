tests
testGsTopazWriter_MultiProject_A

	"ramp up to multiple class extensions from multiple packages and multiple projects"

	"write a single topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

	projectSetDefinition := RwProjectSetDefinition new.

	{1 . 2 } do: [:projectIndex |
		| projectExt |
		projectExt := '_', projectIndex asString.

"create definitions"
		projectDefinition := (RwProjectDefinition
			newForGitBasedProjectNamed: projectName, projectExt)
			addPackageNamed: packageName1, projectExt;
			addPackageNamed: packageName2, projectExt;
			addPackageNamed: packageName3, projectExt;
			setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1, projectExt;
			setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2, projectExt;
			yourself.

		packageDefinition := projectDefinition packageNamed: packageName1, projectExt.

		classDefinition := RwClassDefinition
			newForClassNamed: className1, projectExt
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
			pools: #()
			type: 'normal'.
		classDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method2'
						protocol: 'accessing'
						source: 'method2 ^2');
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize "noop"').
		packageDefinition addClassDefinition: classDefinition.

		classDefinition := RwClassDefinition
			newForClassNamed: className2, projectExt
			super: 'Array'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
			pools: #()
			type: 'normal'.
		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method3'
						protocol: 'accessing'
						source: 'method3 ^3').
		packageDefinition addClassDefinition: classDefinition.

		classDefinition := RwClassDefinition
			newForClassNamed: className3, projectExt
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
			pools: #()
			type: 'normal'.
		classDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method4'
						protocol: 'accessing'
						source: 'method4 ^4').
		packageDefinition addClassDefinition: classDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method1'
						protocol: '*', (packageName2, projectExt) asLowercase
						source: 'method1 ^1').
		packageDefinition := projectDefinition packageNamed: packageName2, projectExt.
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		packageDefinition := projectDefinition packageNamed: packageName3, projectExt.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method5'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'method5 ^5').
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2, projectExt.
		classExtensionDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'initialize "another noop"').
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		projectSetDefinition addDefinition: projectDefinition ].

"export"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.

"validate"
	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsTopazWriterFileContents_Multi_A.
	self assert: x = y