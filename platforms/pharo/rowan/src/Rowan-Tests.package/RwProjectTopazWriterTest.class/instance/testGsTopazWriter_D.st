tests
testGsTopazWriter_D

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

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

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
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
		newForClassNamed: className2
		super: 'Array'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
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
		newForClassNamed: className3
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
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

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1').
	packageDefinition := projectDefinition packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'initialize'
					protocol: '*', packageName3 asLowercase
					source: 'initialize "another noop"').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.


"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		topazFilenamePackageNamesMap: 
			(Dictionary new
				at: packageName1 put: {packageName1};
				at: packageName2 put: {packageName2};
				at: packageName3 put: {packageName3};
				yourself)
		yourself.
	visitor visit: projectSetModification.

	{ packageName1. packageName2. packageName3 }
		do: [:packageName |
			stream := ZnBufferedReadStream on: (repositoryRootPath / packageName, visitor filenameExtension) readStream.
			x := stream contents.
			y := self perform: #'_expectedGsTopazWriterFileContents_D_', (packageName copyWithout: $-).
			self assert: x = y ].