Class {
	#name : 'RwRowanSample2Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample2Test class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , #(#'SampleSymbolDict'  #'RowanSample2' #'RowanSample2_SD')
]

{ #category : 'private' }
RwRowanSample2Test >> _migrationClassMap [

	^ {#(#'RowanSample2' #(#'ivar0')).
	#(#'RowanSample2A' #(#'ivar0' #'a' #'ivar1')).
	#(#'RowanSample2C' #(#'ivar0' #'a' #'ivar1' #'c' #'ivar2')).
	#(#'RowanSample2D' #(#'ivar0' #'a' #'ivar1' #'d' #'ivar2')).
	#(#'RowanSample2B' #(#'ivar0' #'b' #'ivar1')).
	#(#'RowanSample2E' #(#'ivar0' #'b' #'ivar1' #'e' #'ivar2')).
	#(#'RowanSample2F' #(#'ivar0' #'b' #'ivar1' #'f' #'ivar2'))}
]

{ #category : 'tests' }
RwRowanSample2Test >> testRemoveSubclassOfClassWithNewVersion [

	"#RowanSample2 project
		## branch migration_1
			class structure:
				RowanSample2
				    instVarNames: #( ivar0)
				 RowanSample2A
				    instVarNames: #( a ivar1)
				  RowanSample2C
				    instVarNames: #( c ivar2)
				  RowanSample2D
				    instVarNames: #( d ivar2)
				 RowanSample2B
				    instVarNames: #( b ivar1)
				  RowanSample2E
				    instVarNames: #( e ivar2)
				  RowanSample2F
				    instVarNames: #( f ivar2)
		## branch migration_0
			class structure:
				RowanSample2
"

	"load migration_1, then load migration_0 (new version of RowanSample2 and all subclasses deleted"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName rowanSample2Class 
		subclasses projectDef |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := (self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath
		registerProject: false) asDefinition.

	projectDef defaultSymbolDictName: self _symbolDictionaryName.

	gitRootPath := projectDef repositoryRoot.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: self _migration_1_branchName.
	projectDef read; load.

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 6.

	gitTool gitcheckoutIn: gitRootPath with: self _migration_0_branchName.

	(Rowan projectNamed: projectName) asDefinition
		defaultSymbolDictName: self _symbolDictionaryName;
		read;
		load: RwGsInstanceMigrator aggressiveMigration.

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 0
]

{ #category : 'tests' }
RwRowanSample2Test >> testSampleDefaultConfiguration [

	"SampleDefault configuration is an instance of RwProjectConfiguration"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project x projectDef |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := (self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath
		registerProject: false) asDefinition.

	projectDef defaultSymbolDictName: self _symbolDictionaryName.

	gitRootPath := projectDef repositoryRoot.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with:  self _migration_0_branchName.
	projectDef read; load.

	project := RwProject newNamed: projectName.
	self assert: (x := project packageNames) = #('RowanSample2-Core')
]
