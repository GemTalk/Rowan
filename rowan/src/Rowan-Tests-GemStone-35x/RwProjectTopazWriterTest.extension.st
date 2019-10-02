Extension { #name : 'RwProjectTopazWriterTest' }

{ #category : '*rowan-tests-gemstone-35x' }
RwProjectTopazWriterTest >> testGsTopazWriter_A_export [

	"simple case with only two packages and a single set of extensions for a class"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 topazFileReference classExtensionDefinition 
		repositoryRootPath x stream y |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwComponentProjectDefinition newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
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
					source: 'method2 ^2').
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

"export"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	topazFileReference := repositoryRootPath asFileReference / projectName.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.
	projectDefinition exportTopazFormatTo: topazFileReference.

"validate"
	stream := ZnBufferedReadStream on: (topazFileReference, 'gs') readStream.
	x := stream contents.
	y := self _expectedGsTopazWriterFileContents_A.
	self assert: x = y
]

{ #category : '*rowan-tests-gemstone-35x' }
RwProjectTopazWriterTest >> testGsTopazWriter_C_export [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 topazFileReference classExtensionDefinition repositoryRootPath 
		x stream y packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwComponentProjectDefinition newForGitBasedProjectNamed: projectName)
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
					source: 'method2 ^2').
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

"export"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	topazFileReference := repositoryRootPath asFileReference / projectName.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.
	projectDefinition exportTopazFormatTo: topazFileReference usingPackageNamesMap: (Dictionary new
				at: packageName1 put: {packageName1};
				at: packageName2 put: {packageName2};
				at: packageName3 put: {packageName3};
				yourself).

"validate"

	{ packageName1. packageName2. packageName3 }
		do: [:packageName |
			stream := ZnBufferedReadStream on: (repositoryRootPath / packageName, 'gs') readStream.
			x := stream contents.
			y := self perform: #'_expectedGsTopazWriterFileContents_C_', (packageName copyWithout: $-).
			self assert: x = y ].
]
