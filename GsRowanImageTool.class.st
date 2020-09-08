Class {
	#name : 'GsRowanImageTool',
	#superclass : 'GemStoneRowanTool',
	#category : 'GemStone-Rowan-Tools'
}

{ #category : 'generate gs files' }
GsRowanImageTool >> _generateHostAgentBootstrapGsFromSt: repositoryRoot gsSrcRoot: gsSrcRoot topazFileHeader: topazFileHeader [
	| projectSetDefinition projectSetModification specUrl loadSpec resolvedProject topazFileNameMap visitor doCompile |
	doCompile := false.	"not reading in host agent code in host agent environment, so no compilation"

	specUrl := repositoryRoot / 'GemStone64' / 'rowan' / 'specs'
		/ 'gemstoneHostAgentUser.ston'.
	(loadSpec := RwSpecification fromUrl: 'file:' , specUrl pathString)
		projectsHome: repositoryRoot;
		diskUrl: 'file:' , (repositoryRoot / 'GemStone64') pathString;
		yourself.
	resolvedProject := loadSpec resolve
		compileWhileReading: doCompile;
		yourself.

	[ projectSetDefinition := resolvedProject readProjectSet ]
		on: CompileWarning
		do: [ :ex | 
			| str |
			((str := ex asString) includesString: 'not optimized')
				ifTrue: [ ex resume ]
				ifFalse: [ 
					GsFile gciLogServer: str.
					ex pass ] ].

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	topazFileNameMap := Dictionary new.
	resolvedProject packageNames
		do: [ :packageName | (topazFileNameMap at: packageName ifAbsentPut: [ {} ]) add: packageName ].
	visitor := RwGsModificationTopazWriterVisitorV2 new
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.
	^ true
]

{ #category : 'generate gs files' }
GsRowanImageTool >> _generateSystemUserBootstrapGsFromSt: repositoryRoot doCompile: doCompile gsSrcRoot: gsSrcRoot topazFileHeader: topazFileHeader [
	| platformConfigurationAttributes projectSetModification resolvedProject topazFileNameMap visitor gemStoneRowanPackageNames filein4PackageNames combinedPackageNames loadSpec specUrl projectSetDefinition |
	platformConfigurationAttributes := {'common'.
	'gemstone'.
	'bootstraponly'.
	'filein3'.
	'filein4'.
	'ANSI_PortableStreams'}.

	specUrl := repositoryRoot / 'rowan' / 'specs' / 'gemstoneBaseImage.ston'.
	(loadSpec := RwSpecification fromUrl: 'file:' , specUrl pathString)
		projectsHome: repositoryRoot;
		diskUrl: 'file:' , repositoryRoot pathString;
		yourself.

	resolvedProject := loadSpec resolve
		compileWhileReading: doCompile;
		yourself.

	[ 
	projectSetDefinition := resolvedProject
		readProjectSet: platformConfigurationAttributes ]
		on: CompileWarning
		do: [ :ex | 
			| str |
			((str := ex asString) includesString: 'not optimized')
				ifTrue: [ ex resume ]
				ifFalse: [ 
					GsFile gciLogServer: str.
					ex pass ] ].

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.

	gemStoneRowanPackageNames := #('GemStone-Rowan-Extensions-Tools' 'GemStone-Rowan-Tools').
	filein4PackageNames := #('Filein4-CompilerClasses' 'Filein4-Legacy-Streams' 'Filein4-ObsoleteClasses' 'Filein4-Portable-Streams' 'Filein4Rowan').
	combinedPackageNames := gemStoneRowanPackageNames , filein4PackageNames.
	topazFileNameMap := Dictionary new
		at: 'GemStone-Rowan' put: gemStoneRowanPackageNames asSet;
		at: 'Filein4' put: filein4PackageNames asSet;
		yourself.
	resolvedProject packageNames
		do: [ :packageName | 
			(combinedPackageNames includes: packageName)
				ifFalse: [ 
					"write one package per file, except for the GemStone-Rowan and Filein4 packages"
					(topazFileNameMap at: packageName ifAbsentPut: [ {} ]) add: packageName ] ].
	visitor := RwGsModificationTopazWriterVisitorV2 new
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		excludeClassInitializers;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.
	^ true
]

{ #category : 'generate gs files' }
GsRowanImageTool >> _generateX509CoreBootstrapGsFromSt: repositoryRoot doCompile: doCompile gsSrcRoot: gsSrcRoot topazFileHeader: topazFileHeader [
	| platformConfigurationAttributes projectSetDefinition projectSetModification specUrl loadSpec resolvedProject topazFileNameMap visitor |
	platformConfigurationAttributes := {'common'.
	'x509'}.

	specUrl := repositoryRoot / 'rowan' / 'specs' / 'gemstoneBaseImage.ston'.
	(loadSpec := RwSpecification fromUrl: 'file:' , specUrl pathString)
		projectsHome: repositoryRoot;
		diskUrl: 'file:' , repositoryRoot pathString;
		yourself.
	resolvedProject := loadSpec resolve
		compileWhileReading: doCompile;
		yourself.

	[ 
	projectSetDefinition := resolvedProject
		readProjectSet: platformConfigurationAttributes ]
		on: CompileWarning
		do: [ :ex | 
			| str |
			((str := ex asString) includesString: 'not optimized')
				ifTrue: [ ex resume ]
				ifFalse: [ 
					GsFile gciLogServer: str.
					ex pass ] ].

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	topazFileNameMap := Dictionary new.
	((Rowan projectNamed: 'gemstoneBaseImage') _loadedProject components components
		at: 'x509/Kernel') packageNames
		do: [ :packageName | 
			"write one package per file"
			(topazFileNameMap at: packageName ifAbsentPut: [ {} ]) add: packageName ].
	visitor := RwGsModificationTopazWriterVisitorV2 new
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.
	^ true
]

{ #category : 'bootstrap' }
GsRowanImageTool >> adoptGemStone64: specUrl diskUrl: diskUrl projectsHome: projectsHome [

	self adoptGemStone64: specUrl diskUrl: diskUrl projectsHome: projectsHome forUpgrade: false

]

{ #category : 'bootstrap' }
GsRowanImageTool >> adoptGemStone64: specUrl diskUrl: diskUrl projectsHome: projectsHome forUpgrade: forUpgrade [
	"
	Create loaded project (if needed), traverse the package definitions and 
		create loaded packages for each.
	"

	| tracer wasTracing |
	tracer := Rowan projectTools trace.
	wasTracing := tracer isTracing.
	tracer startTracing.

	[ 
	Rowan projectTools adopt
		adoptProjectFromUrl: specUrl
		diskUrl: diskUrl
		projectsHome: projectsHome ]
		on: RwAuditMethodErrorNotification
		do: [ :ex | 
			(ex description = 'Missing loaded method' and: [ forUpgrade ])
				ifTrue: [ 
					| theBehavior |
					"missing loaded method during upgrade, means the method is no longer
						part of image, or was added by end user. Remove the method, for now"
					tracer trace: 'Removing method ' , ex methodPrintString.
					theBehavior := Rowan globalNamed: ex className.
					ex isMetaclass
						ifTrue: [ theBehavior := theBehavior class ].
					theBehavior removeSelector: ex selector.
					ex resume: false	"no audit error" ]
				ifFalse: [ 
					"issue audit error"
					ex resume: true ] ].
	System commit
]

{ #category : 'bootstrap' }
GsRowanImageTool >> createAndPopulateUnPackagedPackage: forUpgrade [
	"
	Then package the unpackaged classes an methods into an unpackaged
		package so that the ENTIRE image is packaged. The UnPackaged 
		should be empty at the end of slow filein ... upgradeImage will be
		expected to manage the UnPackaged package differently.
	"

	| project packagePrefix componentName loadedProject unpackagedName adoptTool userName tracer wasTracing symbolList |
	tracer := Rowan projectTools trace.
	wasTracing := tracer isTracing.
	tracer startTracing.

	project := Rowan newProjectNamed: Rowan unpackagedProjectName.
	componentName := Rowan unpackagedProjectName.
	project
		packageConvention: 'Rowan';
		addNewComponentNamed: componentName.
	packagePrefix := Rowan unpackagedPackagePrefix.
	userName := System myUserProfile userId.
	symbolList := Rowan image symbolList.
	symbolList
		do: [ :symbolDictionary | 
			(#(#'GemStone_Portable_Streams' #'GemStone_Legacy_Streams')
				includes: symbolDictionary name)
				ifFalse: [ 
					| thePackageName |
					"create unpackaged packages for each symbol dictionary"
					thePackageName := packagePrefix , symbolDictionary name asString.
					tracer
						trace: '---Creating Unpackaged package ' , thePackageName printString.
					project
						packageNamed: thePackageName
						ifAbsent: [ 
							project
								addPackageNamed: thePackageName
								toComponentNamed: componentName
								gemstoneDefaultSymbolDictionaryForUser:
									userName -> symbolDictionary name asString ] ] ].
	System commit.
	loadedProject := project load first.	"load the projec"
	System commit.
	unpackagedName := Rowan unpackagedName.
	adoptTool := Rowan packageTools adopt.
	Rowan image symbolList
		do: [ :symbolDictionary | 
			(#(#'GemStone_Portable_Streams' #'GemStone_Legacy_Streams')
				includes: symbolDictionary name)
				ifFalse: [ 
					| thePackage thePackageName |
					thePackageName := packagePrefix , symbolDictionary name asString.
					thePackage := project packageNamed: thePackageName.
					tracer
						trace:
							'---Adopting Unpackaged classes and methods for package '
								, thePackageName printString.
					self
						_classesIn: symbolDictionary
						do: [ :aClass | 
							aClass rowanProjectName = unpackagedName
								ifTrue: [ 
									tracer trace: '	Unpackaged Class ' , aClass name asString printString.
									adoptTool adoptClass: aClass intoPackageNamed: thePackageName ]
								ifFalse: [ 
									| instanceSelectors classSelectors unpackageMethods |
									instanceSelectors := Set new.
									classSelectors := Set new.
									unpackageMethods := false.
									aClass
										methodsDo: [ :selector :method | 
											method rowanProjectName = unpackagedName
												ifTrue: [ 
													tracer
														trace:
															'	Unpackaged method ' , aClass name asString , ' >> ' , selector printString.
													instanceSelectors add: selector.
													unpackageMethods := true ] ].
									aClass class
										methodsDo: [ :selector :method | 
											method rowanProjectName = unpackagedName
												ifTrue: [ 
													tracer
														trace:
															'	Unpackaged method ' , aClass name asString , ' class >> '
																, selector printString.
													classSelectors add: selector.
													unpackageMethods := true ] ].
									unpackageMethods
										ifTrue: [ 
											adoptTool
												adoptClassExtension: aClass
												instanceSelectors: instanceSelectors
												classSelectors: classSelectors
												intoPackageNamed: thePackageName ] ] ].
					System commit ] ].
	wasTracing
		ifFalse: [ tracer stopTracing ]
]

{ #category : 'generate gs files' }
GsRowanImageTool >> generateBootstrapGsFromSt [
	| doCompile |
	doCompile := SessionTemps current
		at: #'ROWAN_COMPILE_WHILE_READING'
		otherwise: true.
	^ self generateBootstrapGsFromSt: doCompile
]

{ #category : 'generate gs files' }
GsRowanImageTool >> generateBootstrapGsFromSt: doCompile [
	| archBase repositoryRoot gsSrcRoot topazFileHeader |
	SessionTemps current at: #'ROWAN_TRACE' put: nil.	"#gciLogServer "
	(archBase := System gemEnvironmentVariable: 'ARCHBASE')
		ifNil: [ Error signal: 'ARCHBASE not defined in environment' ].
	repositoryRoot := (archBase , '/image') asFileReference.
	gsSrcRoot := repositoryRoot pathString , '/bootstrap'.	"where .gs files are written"
	topazFileHeader := '! Copyright (C) GemTalk Systems 1986-2020.  All Rights Reserved.
'.

	self
		_generateSystemUserBootstrapGsFromSt: repositoryRoot
			doCompile: doCompile
			gsSrcRoot: gsSrcRoot
			topazFileHeader: topazFileHeader;
		_generateX509CoreBootstrapGsFromSt: repositoryRoot
			doCompile: doCompile
			gsSrcRoot: gsSrcRoot
			topazFileHeader: topazFileHeader;
		_generateHostAgentBootstrapGsFromSt: repositoryRoot
			gsSrcRoot: gsSrcRoot
			topazFileHeader: topazFileHeader
]

{ #category : 'repository' }
GsRowanImageTool >> newRepositoryRoot: repositoryRoot forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project repositoryRoot: repositoryRoot
]

{ #category : 'repository' }
GsRowanImageTool >> newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'repository' }
GsRowanImageTool >> newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes instanceMigrator: instanceMigrator forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : 'packages' }
GsRowanImageTool >> readRewriteGemStone64Packages: archBase [
	"
		Rowan gemstoneTools image readRewriteGemStone64Packages: '/home/dhenrich/work/j_36x/'
	"

	| repositoryRoot platformConfigurationAttributes specUrl loadSpec resolvedProject |
	SessionTemps current at: #'ROWAN_TRACE' put: nil.	"#gciLogServer "
	repositoryRoot := archBase , '/image'.

	platformConfigurationAttributes := {'common'.
	'gemstone'.
	'bootstraponly'}.

	specUrl := repositoryRoot asFileReference / 'rowan' / 'specs'
		/ 'GemStone64.ston'.
	(loadSpec := RwSpecification fromUrl: 'file:' , specUrl pathString)
		projectsHome: repositoryRoot;
		diskUrl: 'file:' , repositoryRoot;
		yourself.
	resolvedProject := loadSpec resolve.

	[ resolvedProject read: platformConfigurationAttributes ]
		on: CompileWarning
		do: [ :ex | 
			| str |
			((str := ex asString) includesString: 'not optimized')
				ifTrue: [ ex resume ]
				ifFalse: [ 
					GsFile gciLogServer: str.
					ex pass ] ].

	resolvedProject packages
		do: [ :packageDef | 
			| classExtensions |
			"merge class extensions into class definitions and remove class dextension"
			classExtensions := packageDef classExtensions.
			packageDef classDefinitions
				do: [ :classDef | 
					classExtensions
						at: classDef name
						ifPresent: [ :classExtension | 
							classExtension instanceMethodDefinitions
								do: [ :meth | classDef addInstanceMethodDefinition: meth ].
							classExtension classMethodDefinitions
								do: [ :meth | classDef addClassMethodDefinition: meth ].
							packageDef removeClassExtensionDefinition: classExtension ] ] ].

	resolvedProject exportPackages
]
