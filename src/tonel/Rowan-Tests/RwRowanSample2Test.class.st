Class {
	#name : 'RwRowanSample2Test',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

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

{ #category : 'private' }
RwRowanSample2Test >> _rowanSample2SpecificationUrl [

	| rowanSpec |
	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample2.ston'
]

{ #category : 'tests' }
RwRowanSample2Test >> testAutomaticMigration [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after automtic migration the permanent instance variables (a-f) should retain the values, while the moved instance variables (ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample2Class subclasses rowanSample1Spec instanceMap |
	projectName := 'RowanSample2'.
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: true.

	rowanSample1Spec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSample1Spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSample1Spec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: RwGsInstanceMigrator noMigration.

	instanceMap := IdentityKeyValueDictionary new.
	self _migrationClassMap
		do: [ :ar | 
			| className ivs class instance |
			className := ar at: 1.
			class := Rowan globalNamed: className.
			instance := class new.
			ivs := ar at: 2.
			ivs do: [ :ivName | instance perform: ivName , ':' with: ivName ].
			instanceMap at: instance put: ar -> class ].

	gitTool gitcheckoutIn: gitRootPath with: 'migration_2'.
	projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: RwGsInstanceMigrator aggressiveMigration.

	instanceMap
		keysAndValuesDo: [ :instance :assoc | 
			| ivNames ar oldClass className class |
			ar := assoc key.
			oldClass := assoc value.
			className := ar at: 1.
			class := Rowan globalNamed: className.
			self assert: oldClass ~= class.
			self assert: instance class == class.
			ivNames := ar at: 2.
			1 to: ivNames size do: [ :index | 
				| ivName |
				ivName := ivNames at: index.
				self assert: (instance perform: ivName) == ivName ] ]
]

{ #category : 'tests' }
RwRowanSample2Test >> testNoMigration [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample2Class subclasses rowanSample1Spec instanceMap |
	projectName := 'RowanSample2'.
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: true.

	rowanSample1Spec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSample1Spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSample1Spec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

	instanceMap := IdentityKeyValueDictionary new.
	self _migrationClassMap
		do: [ :ar | 
			| className ivs class instance |
			className := ar at: 1.
			class := Rowan globalNamed: className.
			instance := class new.
			ivs := ar at: 2.
			ivs do: [ :ivName | instance perform: ivName , ':' with: ivName ].
			instanceMap at: instance put: ar -> class ].

	gitTool gitcheckoutIn: gitRootPath with: 'migration_2'.
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

	instanceMap
		keysAndValuesDo: [ :instance :assoc | 
			| ivNames ar oldClass className class |
			ar := assoc key.
			oldClass := assoc value.
			className := ar at: 1.
			class := Rowan globalNamed: className.
			self assert: oldClass ~= class.
			self assert: instance class == oldClass.
			ivNames := ar at: 2.
			1 to: ivNames size do: [ :index | 
				| ivName |
				ivName := ivNames at: index.
				self assert: (instance perform: ivName) == ivName ] ]
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

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample2Class subclasses rowanSample1Spec |
	projectName := 'RowanSample2'.
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: true.

	rowanSample1Spec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSample1Spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSample1Spec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 6.

	gitTool gitcheckoutIn: gitRootPath with: 'migration_0'.
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 0
]

{ #category : 'tests' }
RwRowanSample2Test >> testSampleCompoundConfiguration [

	"SampleCompound configuration is an instance of RwProjectCompoundConfiguration"

	"(RwProjectCompoundConfiguration newNamed: 'SampleCompound' for: 'RowanSample2')
		packageNames: #();
		configurationNames: #('SampleCore' 'SampleTest');
		exportToUrl: 'file:$GS_HOME/shared/repos/RowanSample2/configs/';
		yourself.
(RwProjectCompoundConfiguration newNamed: 'SampleCore' for: 'RowanSample2')
		packageNames: #('RowanSample2-Core');
		exportToUrl: 'file:$GS_HOME/shared/repos/RowanSample2/configs/';
		yourself.
(RwProjectCompoundConfiguration newNamed: 'SampleTest' for: 'RowanSample2')
		packageNames: #('RowanSample2-Test');
		exportToUrl: 'file:$GS_HOME/shared/repos/RowanSample2/configs/';
		yourself.
"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample1Spec project x |
	projectName := 'RowanSample2'.
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSample1Spec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSample1Spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSample1Spec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'master'.
	projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'SampleCompound'
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sortWithBlock: [ :a :b | a < b ])
				= #('RowanSample2-Core' 'RowanSample2-Test')
]

{ #category : 'tests' }
RwRowanSample2Test >> testSampleDefaultConfiguration [

	"SampleDefault configuration is an instance of RwProjectConfiguration"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample1Spec project x |
	projectName := 'RowanSample2'.
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSample1Spec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSample1Spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSample1Spec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'master'.
	projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'SampleDefault'
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self assert: (x := project packageNames) = #('RowanSample2-Core')
]

{ #category : 'tests' }
RwRowanSample2Test >> testSampleTestConfiguration [

	"SampleDefault configuration is an instance of RwProjectConfiguration"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample1Spec project x |
	projectName := 'RowanSample2'.
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := RwSpecification fromUrl: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSample1Spec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSample1Spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSample1Spec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'master'.
	projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'SampleTest'
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sortWithBlock: [ :a :b | a < b ])
				= #('RowanSample2-Core' 'RowanSample2-Test')
]
