Extension { #name : 'RwProjectReaderWriterTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectReaderWriterTest >> _projectDefinitionForStructureWriters_A: projectName format: repositoryFormat [

	"multiple class extensions from multiple packages for multiple classes"

	| packageName1 packageName2 resolvedProject classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition packageName3 rowanProject projectsHome componentName |

	self _markForProjectCleanup: projectName.
	componentName := 'Core'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectsHome := self _testRowanProjectsSandbox.


"create definitions"
	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		packageFormat: repositoryFormat;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		diskUrl: 'file://' , (projectsHome / projectName) pathString;
		addNewComponentNamed: componentName;
		addPackagesNamed: {packageName1 . packageName2 . packageName3 } 
			toComponentNamed: componentName;
		yourself.

	resolvedProject loadSpecification
		componentNames: { componentName };
		yourself.

	packageDefinition := resolvedProject packageNamed: packageName1.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: 'this is a test comment for ', className1
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition: 
			(RwMethodDefinition
					newForSelector: #'method6'
					protocol: 'instance accessing'
					source: 'method6 ^6
');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'class accessing'
					source: 'method2 ^2
').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Array'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: 'this is a test comment for ', className2
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3
').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: 'this is a test comment for ', className3
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method8:'
					protocol: 'accessing'
					source: 'method8: ignored ^ignored
');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method9'
					protocol: 'accessing'
					source: 'method9 ^9
');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4
');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method10'
					protocol: 'accessing'
					source: 'method10 ^10
').
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1
').
	packageDefinition := resolvedProject packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := resolvedProject packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5
');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method6'
					protocol: '*', packageName3 asLowercase
					source: 'method6 ^6
').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	^ resolvedProject
]
