! Copyright (C) GemTalk Systems 1986-2020.  All Rights Reserved.
! Class Declarations
! Generated file, do not Edit

doit
(RwAbstractTool
	subclass: 'GemStoneRowanTool'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		immediateInvariant.
true.
%

removeallmethods GemStoneRowanTool
removeallclassmethods GemStoneRowanTool

doit
(GemStoneRowanTool
	subclass: 'GsRowanImageTool'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		immediateInvariant.
true.
%

removeallmethods GsRowanImageTool
removeallclassmethods GsRowanImageTool

doit
(GemStoneRowanTool
	subclass: 'GsTopazRowanTool'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		immediateInvariant.
true.
%

removeallmethods GsTopazRowanTool
removeallclassmethods GsTopazRowanTool

doit
(RwGsModificationTopazWriterVisitorV2
	subclass: 'GsModificationTopazWriterVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		comment: 'GsModificationTopazWriterVisitor is used to export bootstrap
.gs files. The bootstrap .gs are used for loading classes and
methods during image creation and for updating class and 
method definitions during image upgrade.

During image upgrade it is necessary to ensure that all 
methods are removed from each class before installing the 
new methods, since some methods from earlier versions of
GemStone may no longer apply.

Rowan packages are used as the files of record for method
and class source in a GemStone image and the
GsModificationTopazWriterVisitor generates the bootstrap .gs
files from the Rowan packages. Since some classes are 
created in bom.c, while others are created by Rowan class
definitions, it isn''t feasible to remove the existing methods
immediately after the class definition is filed in, so this class
manages the logic for determining when the method removal
code should be inserted in the bootstrap .gs files.';
		immediateInvariant.
true.
%

removeallmethods GsModificationTopazWriterVisitor
removeallclassmethods GsModificationTopazWriterVisitor

doit
(StringKeyValueDictionary
	subclass: 'GsAbstractUnmanagedReport'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		immediateInvariant.
true.
%

removeallmethods GsAbstractUnmanagedReport
removeallclassmethods GsAbstractUnmanagedReport

doit
(GsAbstractUnmanagedReport
	subclass: 'GsUnmanagedClassReport'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		immediateInvariant.
true.
%

removeallmethods GsUnmanagedClassReport
removeallclassmethods GsUnmanagedClassReport

doit
(GsAbstractUnmanagedReport
	subclass: 'GsUnmanagedSymbolDictionariesReport'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #( #logCreation )
)
		category: 'GemStone-Rowan-Tools';
		immediateInvariant.
true.
%

removeallmethods GsUnmanagedSymbolDictionariesReport
removeallclassmethods GsUnmanagedSymbolDictionariesReport

! Class implementation for 'GemStoneRowanTool'

!		Class methods for 'GemStoneRowanTool'

category: 'commands'
classmethod: GemStoneRowanTool
image

	^GsRowanImageTool new
%

category: 'commands'
classmethod: GemStoneRowanTool
topaz

	^GsTopazRowanTool new
%

!		Instance methods for 'GemStoneRowanTool'

category: 'private'
method: GemStoneRowanTool
_classesIn: symbolDictionary do: aBlock
	"traverse all classes in the symbol dictionary, once"

	| coveredClasses |
	coveredClasses := IdentitySet new.
	symbolDictionary
		valuesDo: [ :namedClass | 
			(namedClass isKindOf: Class)
				ifTrue: [ 
					(coveredClasses includes: namedClass)
						ifFalse: [ 
							coveredClasses add: namedClass.
							aBlock value: namedClass ] ] ]
%

! Class implementation for 'GsRowanImageTool'

!		Instance methods for 'GsRowanImageTool'

category: 'bootstrap'
method: GsRowanImageTool
adoptGemStone64: specUrl diskUrl: diskUrl projectsHome: projectsHome

	self adoptGemStone64: specUrl diskUrl: diskUrl projectsHome: projectsHome forUpgrade: false
%

category: 'bootstrap'
method: GsRowanImageTool
adoptGemStone64: specUrl diskUrl: diskUrl projectsHome: projectsHome forUpgrade: forUpgrade
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
%

category: 'bootstrap'
method: GsRowanImageTool
createAndPopulateUnPackagedPackage: forUpgrade
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
%

category: 'generate gs files'
method: GsRowanImageTool
generateBootstrapGsFromSt
	| doCompile |
	doCompile := SessionTemps current
		at: #'ROWAN_COMPILE_WHILE_READING'
		otherwise: true.
	^ self generateBootstrapGsFromSt: doCompile
%

category: 'generate gs files'
method: GsRowanImageTool
generateBootstrapGsFromSt: doCompile
	| archBase repositoryRoot gsSrcRoot topazFileHeader |
	SessionTemps current at: #'ROWAN_TRACE' put: nil.	"#gciLogServer "
	(archBase := System gemEnvironmentVariable: 'ARCHBASE')
		ifNil: [ Error signal: 'ARCHBASE not defined in environment' ].
	repositoryRoot := (archBase , '/image') asFileReference.
	gsSrcRoot := repositoryRoot pathString , '/bootstrap'.	"where .gs files are written"
	topazFileHeader := '! Copyright (C) GemTalk Systems 1986-2020.  All Rights Reserved.
'.

	[ 
	GsModificationTopazWriterVisitor startTrackingClassNames.
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
			topazFileHeader: topazFileHeader ]
		ensure: [ GsModificationTopazWriterVisitor stopTrackingClassNames ]
%

category: 'generateGs command line'
method: GsRowanImageTool
generateGsCommandLine
	| stdout bootstrapDir |
	stdout := GsFile stdout.
	stdout
		lf;
		nextPutAll: 'using ARCHBASE=' , '$ARCHBASE' asFileReference pathString;
		lf;
		lf.
	(self generateGsCommandLine_processArguments: stdout)
		ifFalse: [ 
			"help displayed or unknown argument"
			^ self ].
	bootstrapDir := '$ARCHBASE/image/bootstrap' asFileReference.
	bootstrapDir deleteAllChildren.	"do the dirty work and generate the .gs files"
	Rowan gemstoneTools image generateBootstrapGsFromSt.

	stdout
		nextPutAll: 'Generated files';
		lf;
		nextPutAll: bootstrapDir pathString;
		lf;
		yourself.
	stdout
		nextPutAll:
				(System
						performOnServer:
							'cd ' , bootstrapDir pathString , '; ls -l ' , bootstrapDir pathString);
		lf
%

category: 'generateGs command line'
method: GsRowanImageTool
generateGsCommandLine_processArguments: stdout
	"process command line arguments display help ... answer false if help has been displayed (i.e., exit without generating .gs files)"

	| args scriptArgStart |
	args := System commandLineArguments.
	scriptArgStart := args indexOf: '--'.
	scriptArgStart > 0
		ifTrue: [ 
			| descriptionBlock usageBlock argIndex argsSize |
			descriptionBlock := [
					stdout
						nextPutAll: '  Regenerate $ARCHBASE/image/bootstrap/*.gs files from the tonel files *.st.'; lf;
						nextPutAll: '    Compile each method before writing the .gs files, if a compile error'; lf;
						nextPutAll: '    occurs, you will be able to use the debugger and get a description of'; lf;
						nextPutAll: '    the error.'; lf;
						nextPutAll: '  If you hit a syntax error, you have no choice but to manually edit the'; lf;
						nextPutAll: '    file and fix the error.'; lf;
						nextPutAll: '  If you hit an undefined variable error, you can choose to skip the method'; lf;
						nextPutAll: '    compile altogether (using the -n or --no-compile command line options).'; lf;
						nextPutAll: '    A good choice if you are certain that the variable is defined, i.e, you''ve'; lf;
						nextPutAll: '    already compiled the methods using topaz or jadeite.'; lf;
						nextPutAll: '  If you have manually edited tonel files, it is prudent to compile all of the'; lf;
						nextPutAll: '    methods. Add the list of undefined variables to the command line, where'; lf;
						nextPutAll: '    they will be added to UserGlobals and allow the methods to compile.'; lf;
						lf;
						yourself ].
				usageBlock := [
					stdout
						nextPutAll: 'SYNOPSIS:'; lf;
						nextPutAll: '	generateGs.sh [-n | --no-compile] [-h | --help]'; lf;
						nextPutAll: '	generateGs.sh [UNDEFINED-VARIABLE]...'; lf;
						lf;
						nextPutAll: 'DESCRIPTION';lf;
						nextPutAll: '	Add UNDEFINED-VARIABLE to UserGlobals before compiling methods.'; lf;
						lf;
						nextPutAll: '	-h, --help'; lf;
						nextPutAll: '		Display help and exit'; lf;
						lf;
						nextPutAll: '	-n, --no-compile'; lf;
						nextPutAll: '		Write method source directly to disk without compiling methods.'; lf;
						lf;
						nextPutAll: 'EXAMPLES:'; lf;
						nextPutAll: '	generateGs.sh                     # regenerate .gs files compiling all methods'; lf;
						nextPutAll: '	generateGs.sh --no-compile        # regenerate .gs files skipping method compile'; lf;
						nextPutAll: '	generateGs.sh newIvar NewClass    # add variables to UserGlobals and compile methods'; lf;
						nextPutAll: '	generateGs.sh -h                  # print help text'; lf;
						yourself.
					false ].
			argIndex := scriptArgStart + 1.	"arg after initial --"
			argsSize := args size.
			[ argIndex <= argsSize ] whileTrue: [ 
				| arg |
				arg := args at: argIndex.
				argIndex := argIndex + 1.
				(arg = '--help') | (arg = '-h')
					ifTrue: [ 
						descriptionBlock value.
						^ usageBlock value ].
				(arg = '--no-compile') | (arg = '-n')
					ifTrue: [ SessionTemps current at: #ROWAN_COMPILE_WHILE_READING put: false ]
					ifFalse: [
						(arg beginsWith: '-')
							ifTrue: [
								stdout lf; nextPutAll: 'UNKNOWN argument ', arg printString; lf; lf.
								^ usageBlock value ]
							ifFalse: [
								"add arg to UserGlobals"
								UserGlobals at: arg asSymbol put: nil ] ] ] ].
	"proceed with .gs file generation"
	^ true
%

category: 'repository'
method: GsRowanImageTool
newRepositoryRoot: repositoryRoot forProjectNamed: projectName
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project repositoryRoot: repositoryRoot
%

category: 'repository'
method: GsRowanImageTool
newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes forProjectNamed: projectName
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
%

category: 'repository'
method: GsRowanImageTool
newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes instanceMigrator: instanceMigrator forProjectNamed: projectName
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
		instanceMigrator: instanceMigrator
%

category: 'packages'
method: GsRowanImageTool
readRewriteGemStone64Packages: archBase
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
%

category: 'generate gs files'
method: GsRowanImageTool
_generateHostAgentBootstrapGsFromSt: repositoryRoot gsSrcRoot: gsSrcRoot topazFileHeader: topazFileHeader
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
	visitor := GsModificationTopazWriterVisitor new
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.
	^ true
%

category: 'generate gs files'
method: GsRowanImageTool
_generateSystemUserBootstrapGsFromSt: repositoryRoot doCompile: doCompile gsSrcRoot: gsSrcRoot topazFileHeader: topazFileHeader
	| platformConfigurationAttributes projectSetModification resolvedProject topazFileNameMap visitor gemStoneRowanPackageNames filein4PackageNames loadSpec specUrl projectSetDefinition calculatedGsFilenames expectedGsFilenames combinedPackageNames |
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
	filein4PackageNames := #('Filein4-CompilerClasses' 'Filein4-ObsoleteClasses' 'Filein4Rowan').
	combinedPackageNames := gemStoneRowanPackageNames, filein4PackageNames.
	topazFileNameMap := Dictionary new
		at: 'Filein4' put: filein4PackageNames asSet;
		yourself.
	resolvedProject packageNames
		do: [ :packageName | 
			(combinedPackageNames includes: packageName)
				ifFalse: [ 
					"write one package per file, except for the GemStone-Rowan and Filein4 packages. NOTE: GemStone-Rowan written out separately below"
					(topazFileNameMap at: packageName ifAbsentPut: [ {} ]) add: packageName ] ].
	calculatedGsFilenames := topazFileNameMap keys sort asArray.
	expectedGsFilenames := self _systemUserBootstrapFilenamesOrdered sort asArray.
	calculatedGsFilenames = expectedGsFilenames
		ifFalse: [ 
			self
				error:
					'There is a mismatch between the calculated bootstrap gs file names '
						, calculatedGsFilenames printString
						, ' and the expected bootstrap .gs file names '
						, expectedGsFilenames printString
						,
							'. Edit the list in GsRowanImageTool>>_systemUserBootstrapFilenamesOrdered.' ].
	visitor := GsModificationTopazWriterVisitor new
		fileNamesInFileInOrder: self _systemUserBootstrapFilenamesOrdered;
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		excludeClassInitializers: true;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.

	"GemStone-Rowan .gs file should only put removeall* commands after class definitions - no upgrade of extent0.rowan.dbf ... yet"
	topazFileNameMap := Dictionary new
		at: 'GemStone-Rowan' put: gemStoneRowanPackageNames asSet;
		yourself.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.

	^ true
%

category: 'generate gs files'
method: GsRowanImageTool
_generateX509CoreBootstrapGsFromSt: repositoryRoot doCompile: doCompile gsSrcRoot: gsSrcRoot topazFileHeader: topazFileHeader
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
	visitor := GsModificationTopazWriterVisitor new
		logCreation: true;
		repositoryRootPath: gsSrcRoot;
		topazFilenamePackageNamesMap: topazFileNameMap;
		topazFileHeader: topazFileHeader;
		yourself.
	visitor visit: projectSetModification.
	^ true
%

category: 'generate gs files'
method: GsRowanImageTool
_systemUserBootstrapFilenamesOrdered
	"The order that bootstrap .gs files are loaded needs to be 
		known so that the placement of the remove all methods 
		commands is correct ... remove all methods commands 
		are inserted before the first occurence of a class in the 
		bootstrap .gs files, whether a method definition or class 
		definition"

	^ {'Filein1A'.
	'Filein1A-BootstrapOnly'.
	'Filein1B'.
	'Filein1C'.
	'Filein1C-BootstrapOnly'.
	'Filein1D-ObsoleteClasses'.
	'Filein2A'.
	'Filein2CInit'.
	'Filein2Streams'.
	'Filein3A'.
	'Filein3B'.
	'Filein3B-BootstrapOnly'.
	'Filein3C-ObsoleteClasses'.
	'Filein3D-CompilerClasses'.
	'Filein3E-CompilerClasses'.
	'Filein3Init'.
	'Filein4'.
	'Filein4-Portable-Streams'}
%

! Class implementation for 'GsTopazRowanTool'

!		Instance methods for 'GsTopazRowanTool'

category: 'packages'
method: GsTopazRowanTool
addNewPackageNamed: packageName forProjectNamed: projectName
	"add a new package to the named loaded project"

	^ self
		addNewPackageNamed: packageName
		forProjectNamed: projectName
		toComponentNamed: self _defaultComponentName
%

category: 'packages'
method: GsTopazRowanTool
addNewPackageNamed: packageName forProjectNamed: projectName inSybolDictionaryNamed: symbolDictionaryName toComponentNamed: componentName
	"add a new package to the named loaded project"

	^ (Rowan projectNamed: projectName)
		addNewPackageNamed: packageName
		inSybolDictionaryNamed: symbolDictionaryName
		toComponentNamed: componentName
%

category: 'packages'
method: GsTopazRowanTool
addNewPackageNamed: packageName forProjectNamed: projectName toComponentNamed: componentName
	"add a new package to the named loaded project"

	^ (Rowan projectNamed: projectName)
		addNewPackageNamed: packageName
		toComponentNamed: componentName
%

category: 'components'
method: GsTopazRowanTool
addTopLevelComponentNamed: componentName forProjectNamed: projectName
	"Add the named component to the named project and add the component name to the load specification"

	self
		addTopLevelComponentNamed: componentName
		forProjectNamed: projectName
		condition: self _defaultComponentCondition
%

category: 'components'
method: GsTopazRowanTool
addTopLevelComponentNamed: componentName forProjectNamed: projectName condition: condition
	"Add the named component to the named project and add the component name to the load specification
		with the given condition"

	(Rowan projectNamed: projectName)
		addTopLevelComponentNamed: componentName
		condition: condition
%

category: 'projects'
method: GsTopazRowanTool
createNewLoadedProject: projectName in: parentDirectory componentName: componentName packageNames: packageNames defaultSymbolDictionaryName: defaultSymbolDictionaryName
	"Create a new loaded project with the given attributes, using the default project type, packageFormat and packageConvention.
		The project is created, written to disk, loaded into the image, and the project is set as the current topaz project. 

	Return the newly created project (instance of RwProject)"

	^ self
		createNewLoadedProject: projectName
		in: parentDirectory
		type: self _defaultProjectType
		packageFormat: self _defaultPackageFormat
		packageConvention: self _defaultPackageConvention
		componentName: componentName
		packageNames: packageNames
		defaultSymbolDictionaryName: defaultSymbolDictionaryName
%

category: 'projects'
method: GsTopazRowanTool
createNewLoadedProject: projectName in: parentDirectory packageNames: packageNames
	"Create a new loaded project with the given attributes, using the default project type, packageFormat, packageConvention,
		componentName and defaultSymbolDictionaryName.
		The project is created, written to disk, loaded into the image, and the project is set as the current topaz project. 

	Return the newly created project (instance of RwProject)"

	^ self
		createNewLoadedProject: projectName
		in: parentDirectory
		type: self _defaultProjectType
		packageFormat: self _defaultPackageFormat
		packageConvention: self _defaultPackageConvention
		componentName: self _defaultComponentName
		packageNames: packageNames
		defaultSymbolDictionaryName: self _defaultDefaultSymbolDictionaryName
%

category: 'projects'
method: GsTopazRowanTool
createNewLoadedProject: projectName in: projectsHome type: repoType packageFormat: packageFormat packageConvention: packageConvention componentName: componentName packageNames: packageNames defaultSymbolDictionaryName: defaultSymbolDictionaryName
	"Create a new loaded project with the given attributes.
		The project is created, written to disk, loaded into the image, and the project is set as the current topaz project. 

	Return the newly created project (instance of RwProject)"

	| project |
	project := (Rowan newProjectNamed: projectName)
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictionaryName;
		repoType: repoType;
		packageFormat: packageFormat;
		packageConvention: packageConvention;
		addTopLevelComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself.
	self currentTopazProjectName: projectName.
	^ project resolve write load at: 1
%

category: 'packages'
method: GsTopazRowanTool
currentTopazPackageName
	"Return the current topaz package name or nil"

	^ SessionTemps current at: self _currentTopazPackageKey otherwise: nil
%

category: 'packages'
method: GsTopazRowanTool
currentTopazPackageName: packageNameOrNil
	"Set the current topaz package name. New methods and classes created in the topaz session 
		will be added to the named package. If nil, new methods and classes will be unmanaged."

	SessionTemps current at: self _currentTopazPackageKey put: packageNameOrNil
%

category: 'projects'
method: GsTopazRowanTool
currentTopazProjectName
	"answer the name of the current project"

	^ SessionTemps current at: self _currentTopazProjectKey otherwise: nil
%

category: 'projects'
method: GsTopazRowanTool
currentTopazProjectName: projectNameOrNil
	"set the name of the current project"

	SessionTemps current at: self _currentTopazProjectKey put: projectNameOrNil
%

category: 'components'
method: GsTopazRowanTool
exportComponentsForProject: projectName
	"save the current components for the named project to disk"

	^ (Rowan projectNamed: projectName) defined resolve exportComponents
%

category: 'load specs'
method: GsTopazRowanTool
exportLoadSpecificationForProject: projectName
	"save the current load specification for the named project to disk"

	^ (Rowan projectNamed: projectName) defined resolve exportLoadSpecification
%

category: 'packages'
method: GsTopazRowanTool
exportPackagesForProject: projectName
	"save the currently modified packages in the named project to disk"

	^ (Rowan projectNamed: projectName) defined resolve exportPackages
%

category: 'projects'
method: GsTopazRowanTool
exportProjectNamed: projectName inTopazFormatTo: filePath
	"export the loaded packages in the named project to a topaz format file named filePath"

	^ (Rowan projectNamed: projectName)
		exportTopazFormatTo: filePath
		logClassCreation: true
		excludeClassInitializers: false
		excludeRemoveAllMethods: false
%

category: 'git support'
method: GsTopazRowanTool
gitCheckoutProject: projectName branchOrSHA: branchOrSHA
	"do a git checkout of the given branchOrCommit for the given project"

	^ (Rowan projectNamed: projectName) gitCheckout: branchOrSHA
%

category: 'git support'
method: GsTopazRowanTool
gitCommitProject: projectName commitComment: comment
	"do a git commit of the given project with the given commit comment"

	^ (Rowan projectNamed: projectName) gitCommit: comment
%

category: 'git support'
method: GsTopazRowanTool
gitCreateBranchProject: projectName branchName: branchName
	"do a git checkout of the given branchOrCommit for the given project"

	^ (Rowan projectNamed: projectName) gitCreateBranch: branchName
%

category: 'git support'
method: GsTopazRowanTool
gitLogProject: projectName
	"Return the git log of the project"

	^ self gitLogProject: projectName logLimit: 25
%

category: 'git support'
method: GsTopazRowanTool
gitLogProject: projectName logLimit: logLimit
	"Return the git log of the project"

	^ (Rowan projectNamed: projectName) gitLog: logLimit
%

category: 'git support'
method: GsTopazRowanTool
gitPullProject: projectName remote: remoteName branch: branchName
	"do a git pull for the given projectl remote and branch"

	^ (Rowan projectNamed: projectName)
		gitPullRemote: remoteName
		branch: branchName
%

category: 'git support'
method: GsTopazRowanTool
gitPushProject: projectName remote: remoteName branch: branchName
	"do a git push for the given projectl remote and branch"

	^ (Rowan projectNamed: projectName)
		gitPushRemote: remoteName
		branch: branchName
%

category: 'git support'
method: GsTopazRowanTool
gitShortStatusProject: projectName
	"Return the git short status of the project; should be empty unless the working directory has been modified"

	^ (Rowan projectNamed: projectName) gitShortStatus
%

category: 'git support'
method: GsTopazRowanTool
gitStatusProject: projectName
	"Return the git status of the project"

	^ (Rowan projectNamed: projectName) gitStatus
%

category: 'reports'
method: GsTopazRowanTool
listPackagesForProjectNamed: projectName
	"Return a list of the currently visible projects"

	^ (Rowan projectNamed: projectName) packageNames
%

category: 'reports'
method: GsTopazRowanTool
listProjects
	"Return a list of the currently visible projects"

	^ Rowan projectNames
%

category: 'projects'
method: GsTopazRowanTool
loadProjectFromUrl: loadSpecUrl projectsHome: projectsHome
	"read the load specification from the given url; resolve the spec to clone the project (if needed) and
		read the packages from disk based on the default component names and default conditional attributes."

	^ self
		loadProjectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: nil
		customConditionalAttributes: nil
%

category: 'projects'
method: GsTopazRowanTool
loadProjectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNamesOrNil customConditionalAttributes: customConditionalAttributesOrNil
	"read the load specification from the given url; resolve the spec to clone the project (if needed) and
		read the packages from disk based on the listed component names (if nil, use the component names
		defined in load spec) and conditional attributes (if nil, use the conditional attributes defined in the
		load spec)."

	| loadSpec resolvedProject rwResolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		yourself.
	componentNamesOrNil ifNotNil: [ loadSpec componentNames: componentNamesOrNil ].
	customConditionalAttributesOrNil
		ifNotNil: [ loadSpec customConditionalAttributes: customConditionalAttributesOrNil ].
	resolvedProject := loadSpec resolve.
	rwResolvedProject := (RwResolvedProject newNamed: resolvedProject name)
		_resolvedProject: resolvedProject resolve;
		yourself.
	^ rwResolvedProject load at: 1
%

category: 'classes'
method: GsTopazRowanTool
moveClass: class toPackageNamed: packageName
	"Move class to <packageName>, whether or not it has been packaged. The methods in the class that are in the
		original package of the class are also moved to the new package. If the class was originally unpackaged,
		then only unpackaged methods (class and instance side) are moved to the new package."

	| loadedPackage |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package ' , packageName printString , ' does not exist' ].
	class rwMoveClassToPackage: packageName
%

category: 'methods'
method: GsTopazRowanTool
moveMethod: method toPackageNamed: packageName
	"Move the method into <packageName>, whether or not it has been packaged"

	| loadedPackage theBehavior |
	(theBehavior := method inClass)
		ifNil: [ self error: 'An anonymous method cannot be packaged' ].
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package ' , packageName printString , ' does not exist' ].
	theBehavior rwMoveMethod: method selector toPackage: packageName
%

category: 'projects'
method: GsTopazRowanTool
reloadProjectNamed: projectName
	"reload the named project and dependent projects from disk. The components and packages are reread from disk based on the 
		settings in the loaded load specification and then loaded into the image.

		Return the list of projects (RwProject) that were loaded."

	^ (Rowan projectNamed: projectName) loadProjectSet
%

category: 'packages'
method: GsTopazRowanTool
removePackageNamed: packageName
	"remove the package from the loaded project associated with the package"

	| loadedPackage projectName |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package ' , packageName printString , ' was not found' ].
	projectName := loadedPackage loadedProject name.
	^ self removePackageNamed: packageName fromProjectNamed: projectName
%

category: 'packages'
method: GsTopazRowanTool
removePackageNamed: packageName fromProjectNamed: projectName
	"remove the package from the named project"

	^ (Rowan projectNamed: projectName) removePackageNamed: packageName
%

category: 'projects'
method: GsTopazRowanTool
unloadProjectNamed: projectName
	"unload the named project"

	^ (Rowan projectNamed: projectName) unload
%

category: 'reports'
method: GsTopazRowanTool
unmanagedClassesAndMethodsReportForClass: aClass
	| packagedReport unpackagedReport details classDetails theClassDetails |
	packagedReport := WriteStream on: String new.
	unpackagedReport := WriteStream on: String new.
	details := GsUnmanagedClassReport new.
	classDetails := details
		at: 'classDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	theClassDetails := classDetails
		at: aClass name
		ifAbsentPut: [ StringKeyValueDictionary new ].
	self
		_unmanagedClassesAndMethodsReportForClass: aClass
		details: theClassDetails
		packaged: packagedReport
		unpackaged: unpackagedReport.
	details
		at: 'packagedReport' put: packagedReport contents;
		at: 'unpackagedReport' put: unpackagedReport contents;
		yourself.
	^ details
%

category: 'reports'
method: GsTopazRowanTool
unmanagedClassesAndMethodsReportForSymbolDictionaries: symbolDictionaries
	| packagedReport unpackagedReport details symbolDictionariesDetails |
	packagedReport := WriteStream on: String new.
	unpackagedReport := WriteStream on: String new.
	details := GsUnmanagedSymbolDictionariesReport new.
	symbolDictionariesDetails := details
		at: 'symbolDictionaryDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	self
		_unmanagedClassesAndMethodsReportForSymbolDictionaries: symbolDictionaries
		details: symbolDictionariesDetails
		packaged: packagedReport
		unpackaged: unpackagedReport.
	details
		at: 'packagedReport' put: packagedReport contents;
		at: 'unpackagedReport' put: unpackagedReport contents;
		yourself.
	^ details
%

category: 'reports'
method: GsTopazRowanTool
unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary
	| packagedReport unpackagedReport details symbolDictionariesDetails symbolDictionaryDetails |
	packagedReport := WriteStream on: String new.
	unpackagedReport := WriteStream on: String new.
	details := GsUnmanagedSymbolDictionariesReport new.
	symbolDictionariesDetails := details
		at: 'symbolDictionaryDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	symbolDictionaryDetails := symbolDictionariesDetails
		at: symbolDictionary name asString
		ifAbsentPut: [ StringKeyValueDictionary new ].
	symbolDictionaryDetails at: 'symbolDictionary' put: symbolDictionary.
	self
		_unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary
		details: symbolDictionaryDetails
		packaged: packagedReport
		unpackaged: unpackagedReport.
	details
		at: 'packagedReport' put: packagedReport contents;
		at: 'unpackagedReport' put: unpackagedReport contents;
		yourself.
	^ details
%

category: 'classes'
method: GsTopazRowanTool
unpackageClass: class
	"unpackage the given class and all of the methods in the class (instance and class side) that are in the 
		same package as the class definition. Do nothing if the class definition is not packaged."

	Rowan projectTools browser unpackageClass: class
%

category: 'methods'
method: GsTopazRowanTool
unpackageMethod: method
	"unpackage the given method, while leaving the method installed in the image"

	| theBehavior |
	(theBehavior := method inClass)
		ifNil: [ self error: 'An anonymous method cannot be unpackaged' ].
	Rowan projectTools browser unpackageMethod: method
%

category: 'private'
method: GsTopazRowanTool
_currentTopazPackageKey
	^ #'RowanTopazCurrentPackageName'
%

category: 'private'
method: GsTopazRowanTool
_currentTopazProjectKey
	^#'RowanTopazCurrentProjectName'
%

category: 'private'
method: GsTopazRowanTool
_defaultComponentCondition
	^ 'common'
%

category: 'private'
method: GsTopazRowanTool
_defaultComponentName
	^ 'Core'
%

category: 'private'
method: GsTopazRowanTool
_defaultDefaultSymbolDictionaryName
	^ 'UserGlobals'
%

category: 'private'
method: GsTopazRowanTool
_defaultPackageConvention
	^ 'Rowan'
%

category: 'private'
method: GsTopazRowanTool
_defaultPackageFormat
	^ 'tonel'
%

category: 'private'
method: GsTopazRowanTool
_defaultProjectType
	^ #'disk'
%

category: 'reports'
method: GsTopazRowanTool
_unmanagedClassesAndMethodsReportForClass: aClass details: details packaged: packagedReport unpackaged: unpackagedReport
	| unpackagedName packageName methodDetails unpackagedClassLogged packagedClassLogged |
	unpackagedName := Rowan unpackagedName.

	details at: 'class' put: aClass.
	packageName := aClass rowanPackageName.
	details at: 'packageName' put: packageName.
	packagedClassLogged := unpackagedClassLogged := false.
	packageName = unpackagedName
		ifTrue: [ 
			unpackagedClassLogged := true.
			unpackagedReport
				nextPutAll: '	Class: ' , aClass name asString;
				nextPutAll: ' unpackaged';
				lf ]
		ifFalse: [ 
			packagedClassLogged := true.
			packagedReport
				nextPutAll: '	Class: ' , aClass name asString;
				nextPutAll: ' packaged in ' , packageName;
				lf ].
	methodDetails := details
		at: 'instanceMethodDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	aClass
		methodsDo: [ :selector :method | 
			| methodDetail |
			methodDetail := methodDetails
				at: selector asString
				put: StringKeyValueDictionary new.
			packageName := method rowanPackageName.
			methodDetail
				at: 'method' put: method;
				at: 'packageName' put: packageName.
			packageName = unpackagedName
				ifTrue: [ 
					unpackagedClassLogged
						ifFalse: [ 
							unpackagedClassLogged := true.
							unpackagedReport
								nextPutAll: '	Class: ' , aClass name asString;
								lf ].
					unpackagedReport
						nextPutAll: '		Method: ' , selector asString;
						nextPutAll: ' unpackaged';
						lf ]
				ifFalse: [ 
					packagedClassLogged
						ifFalse: [ 
							packagedClassLogged := true.
							packagedReport
								nextPutAll: '	Class: ' , aClass name asString;
								lf ].
					packagedReport
						nextPutAll: '		Method: ' , selector asString;
						nextPutAll: ' packaged in ' , packageName;
						lf ] ].
	methodDetails := details
		at: 'classMethodDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	aClass class
		methodsDo: [ :selector :method | 
			| methodDetail |
			methodDetail := methodDetails
				at: selector asString
				put: StringKeyValueDictionary new.
			packageName := method rowanPackageName.
			methodDetail
				at: 'method' put: method;
				at: 'packageName' put: packageName.
			packageName = unpackagedName
				ifTrue: [ 
					unpackagedReport
						nextPutAll: '		Class method: ' , selector asString;
						nextPutAll: ' unpackaged';
						lf ]
				ifFalse: [ 
					packagedReport
						nextPutAll: '		Class method: ' , selector asString;
						nextPutAll: ' packaged in ' , packageName;
						lf ] ]
%

category: 'reports'
method: GsTopazRowanTool
_unmanagedClassesAndMethodsReportForSymbolDictionaries: symbolDictionaries details: details packaged: packagedReport unpackaged: unpackagedReport
	| unpackagedName |
	unpackagedName := Rowan unpackagedName.
	symbolDictionaries
		do: [ :symbolDictionary | 
			| symbolDictionaryDetails |
			symbolDictionaryDetails := details
				at: symbolDictionary name asString
				ifAbsentPut: [ StringKeyValueDictionary new ].
			symbolDictionaryDetails at: 'symbolDictionary' put: symbolDictionary.
			self
				_unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary
				details: symbolDictionaryDetails
				packaged: packagedReport
				unpackaged: unpackagedReport ]
%

category: 'reports'
method: GsTopazRowanTool
_unmanagedClassesAndMethodsReportForSymbolDictionary: symbolDictionary details: details packaged: packagedReport unpackaged: unpackagedReport
	| unpackagedName classDetails |
	unpackagedName := Rowan unpackagedName.
	classDetails := details
		at: 'classDetails'
		ifAbsentPut: [ StringKeyValueDictionary new ].
	self
		_classesIn: symbolDictionary
		do: [ :aClass | 
			| theClassDetails |
			theClassDetails := classDetails
				at: aClass name
				ifAbsentPut: [ StringKeyValueDictionary new ].
			self
				_unmanagedClassesAndMethodsReportForClass: aClass
				details: theClassDetails
				packaged: packagedReport
				unpackaged: unpackagedReport ]
%

! Class implementation for 'GsModificationTopazWriterVisitor'

!		Class methods for 'GsModificationTopazWriterVisitor'

category: 'session management'
classmethod: GsModificationTopazWriterVisitor
classTrackingSet
	^ SessionTemps current at: self _sessionTempsKey ifAbsent: [  ]
%

category: 'session management'
classmethod: GsModificationTopazWriterVisitor
isTrackingClassNames
	^ SessionTemps current includesKey: self _sessionTempsKey
%

category: 'session management'
classmethod: GsModificationTopazWriterVisitor
startTrackingClassNames
	SessionTemps current at: self _sessionTempsKey put: Set new
%

category: 'session management'
classmethod: GsModificationTopazWriterVisitor
stopTrackingClassNames
	SessionTemps current removeKey: self _sessionTempsKey ifAbsent: [  ]
%

category: 'session management'
classmethod: GsModificationTopazWriterVisitor
_sessionTempsKey

	^#'GsModificationTopazWriterVisitor_classTrackingSet'
%

!		Instance methods for 'GsModificationTopazWriterVisitor'

category: 'private exporting'
method: GsModificationTopazWriterVisitor
classTrackingSet
	^ self class classTrackingSet
%

category: 'private exporting'
method: GsModificationTopazWriterVisitor
_fileOutMethod: methodDefinition forClass: className isMeta: isMeta on: aStream
	"conditionaly remove all methods for class BEFORE first method definition"

	self _fileoutRemoveAllMethodsFor: className on: aStream.
	super
		_fileOutMethod: methodDefinition
		forClass: className
		isMeta: isMeta
		on: aStream
%

category: 'private exporting'
method: GsModificationTopazWriterVisitor
_fileoutRemoveAllMethodsFor: className on: aStream
	"export remove all methods commands if removeall methods
		are enabled and this is first time the class has been seen"

	self classTrackingSet
		ifNotNil: [ :classTrackingSet | 
			(classTrackingSet includes: className)
				ifFalse: [ 
					super _fileoutRemoveAllMethodsFor: className on: aStream.
					classTrackingSet add: className ] ]
%

! Class implementation for 'GsAbstractUnmanagedReport'

!		Instance methods for 'GsAbstractUnmanagedReport'

category: 'accessing'
method: GsAbstractUnmanagedReport
packagedReport
	^ self at: 'packagedReport'
%

category: 'accessing'
method: GsAbstractUnmanagedReport
unpackagedReport
	^ self at: 'unpackagedReport'
%

! Class implementation for 'GsUnmanagedClassReport'

!		Instance methods for 'GsUnmanagedClassReport'

category: 'enumerating'
method: GsUnmanagedClassReport
unmanagedClassesDo: aBlock
	| unpackagedName |
	unpackagedName := Rowan unpackagedName.
	(self at: 'classDetails')
		keysAndValuesDo: [ :className :classDetails | 
			(classDetails at: 'packageName') = unpackagedName
				ifTrue: [ aBlock cull: classDetails ] ]
%

category: 'enumerating'
method: GsUnmanagedClassReport
unmanagedMethodsDo: aBlock
	| unpackagedName |
	unpackagedName := Rowan unpackagedName.
	(self at: 'classDetails')
		keysAndValuesDo: [ :className :classDetails | 
			(classDetails at: 'instanceMethodDetails')
				keysAndValuesDo: [ :methodSelector :methodDetails | 
					(methodDetails at: 'packageName') = unpackagedName
						ifTrue: [ aBlock cull: methodDetails cull: classDetails ] ].
			(classDetails at: 'classMethodDetails')
				keysAndValuesDo: [ :methodSelector :methodDetails | 
					(methodDetails at: 'packageName') = unpackagedName
						ifTrue: [ aBlock cull: methodDetails cull: classDetails ] ] ]
%

! Class implementation for 'GsUnmanagedSymbolDictionariesReport'

!		Instance methods for 'GsUnmanagedSymbolDictionariesReport'

category: 'enumerating'
method: GsUnmanagedSymbolDictionariesReport
unmanagedClassesDo: aBlock
	| unpackagedName |
	unpackagedName := Rowan unpackagedName.
	(self at: 'symbolDictionaryDetails')
		keysAndValuesDo: [ :symbolDictionaryName :symbolDictionaryDetail | 
			(symbolDictionaryDetail at: 'classDetails')
				keysAndValuesDo: [ :className :classDetail | 
					(classDetail at: 'packageName') = unpackagedName
						ifTrue: [ aBlock cull: classDetail cull: symbolDictionaryDetail ] ] ]
%

category: 'enumerating'
method: GsUnmanagedSymbolDictionariesReport
unmanagedMethodsDo: aBlock
	| unpackagedName |
	unpackagedName := Rowan unpackagedName.
	(self at: 'symbolDictionaryDetails')
		keysAndValuesDo: [ :symbolDictionaryName :symbolDictionaryDetail | 
			(symbolDictionaryDetail at: 'classDetails')
				keysAndValuesDo: [ :className :classDetail | 
					(classDetail at: 'instanceMethodDetails')
						keysAndValuesDo: [ :methodSelector :methodDetail | 
							(methodDetail at: 'packageName') = unpackagedName
								ifTrue: [ aBlock cull: methodDetail cull: classDetail cull: symbolDictionaryDetail ] ].
					(classDetail at: 'classMethodDetails')
						keysAndValuesDo: [ :methodSelector :methodDetail | 
							(methodDetail at: 'packageName') = unpackagedName
								ifTrue: [ aBlock cull: methodDetail cull: classDetail cull: symbolDictionaryDetail ] ] ] ]
%

! Class extensions for 'Rowan'

!		Class methods for 'Rowan'

category: 'rowan-gemstone-core'
classmethod: Rowan
gemstoneTools

	^self platform gemstoneTools
%

! Class extensions for 'RwGsPlatform'

!		Instance methods for 'RwGsPlatform'

category: 'gemstone-rowan-extensions-toools'
method: RwGsPlatform
gemstoneTools
	"Answer the platform-specific class for project tools"

	^GemStoneRowanTool
%

