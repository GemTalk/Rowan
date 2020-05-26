Class {
	#name : 'RwGsImageTool',
	#superclass : 'RwGemStoneTool',
	#category : 'Rowan-Tools-GemStone'
}

{ #category : 'bootstrap' }
RwGsImageTool >> adoptGemStone64: specUrl projectsHome: projectsHome [
	"
	Create loaded project (if needed), traverse the package definitions and 
		create loaded packages for each.
	
	Then package the unpackaged classes an methods into an UnPackaged
		package so that the ENTIRE image is packaged. The UnPackaged 
		should be empty at the end of slow filein ... upgradeImage will be
		expected to manage the UnPackaged package differently.
	"

	| project packagePrefix componentName loadedProject unpackagedName adoptTool userName tracer wasTracing symbolList |
	tracer := Rowan projectTools trace.
	wasTracing := tracer isTracing.
	tracer startTracing.
	Rowan projectTools adopt
		adoptProjectFromUrl: specUrl
		diskUrl: 'file:' , projectsHome
		projectsHome: projectsHome.
	project := Rowan newProjectNamed: 'UnPackaged'.
	componentName := 'UnPackaged'.
	project addNewComponentNamed: componentName.
	packagePrefix := 'UnPackaged-'.
	userName := System myUserProfile userId.
	symbolList := Rowan image symbolList.
	symbolList
		do: [ :symbolDictionary | 
			(#(#'GemStone_Portable_Streams' #'GemStone_Legacy_Streams')
				includes: symbolDictionary name)
				ifTrue: [ 
					"defer handling the classes in this symbol dictionary until a later date"
					tracer
						trace:
							'>>>SKIP Unpackaged package for symbolDictionary '
								, symbolDictionary name asString printString ]
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
	loadedProject := project load first.	"load the projec"
	unpackagedName := Rowan unpackagedName.
	adoptTool := Rowan packageTools adopt.
	Rowan image symbolList
		do: [ :symbolDictionary | 
			(#(#'GemStone_Portable_Streams' #'GemStone_Legacy_Streams')
				includes: symbolDictionary name)
				ifTrue: [ 
					"defer handling the classes in this symbol dictionary until a later date"
					tracer
						trace:
							'>>>SKIPPING classes and methods for symbolDictionary '
								, symbolDictionary name asString printString ]
				ifFalse: [ 
					| thePackage thePackageName |
					thePackageName := packagePrefix , symbolDictionary name asString.
					thePackage := project packageNamed: thePackageName.
					tracer
						trace:
							'---Adopting Unpackaged classes and methods for package '
								, thePackageName printString.
					symbolDictionary
						classesDo: [ :aUserProfile :aSymbolDictionary :aClass | 
							(symbolList dictionariesAndSymbolsOf: aClass)
								do: [ :ar | 
									| classSymbolDictionary |
									classSymbolDictionary := ar at: 1.
									tracer
										trace:
											'	--- symbol dictionary name ' , classSymbolDictionary name asString printString
												, ' for class named: ' , aClass name asString printString.
									classSymbolDictionary == symbolDictionary
										ifTrue: [ 
											aClass rowanProjectName = unpackagedName
												ifTrue: [ 
													tracer trace: '	Unpackaged Class ' , aClass name asString printString.
													adoptTool
														adoptClassNamed: aClass name asString
														intoPackageNamed: thePackageName ]
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
																adoptClassExtensionNamed: aClass name asString
																instanceSelectors: instanceSelectors
																classSelectors: classSelectors
																intoPackageNamed: thePackageName ] ] ]
										ifFalse: [ 
											tracer
												trace:
													'	>>> symbol dictionary mismatch ('
														, classSymbolDictionary name asString printString , ' instead of '
														, symbolDictionary name asString printString , ') for class '
														, aClass name asString printString ] ] ] ] ].
	wasTracing
		ifFalse: [ tracer stopTracing ]
]

{ #category : 'repository' }
RwGsImageTool >> newRepositoryRoot: repositoryRoot forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project repositoryRoot: repositoryRoot
]

{ #category : 'repository' }
RwGsImageTool >> newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'repository' }
RwGsImageTool >> newRepositoryRoot: repositoryRoot platformConditionalAttributes: platformConditionalAttributes instanceMigrator: instanceMigrator forProjectNamed: projectName [
	"change the repositoryRoot and then load from disk, includes enbedded projects"

	| project |
	project := Rowan projectNamed: projectName.
	^ project
		repositoryRoot: repositoryRoot
		platformConditionalAttributes: platformConditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : 'packages' }
RwGsImageTool >> readRewriteGemStone64Packages: archBase [
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
