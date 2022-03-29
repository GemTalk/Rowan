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
RwRowanSample2Test >> _rowanSample2_bitbucketSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath
		, '/test/specs/RowanSample2_bitbucket.ston'

]

{ #category : 'private' }
RwRowanSample2Test >> _rowanSample2_gitlabSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath
		, '/test/specs/RowanSample2_gitlab.ston'

]

{ #category : 'private' }
RwRowanSample2Test >> _rowanSample2_gitoliteSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath
		, '/test/specs/RowanSample2_gitolite.ston'

]

{ #category : 'private' }
RwRowanSample2Test >> _rowanSample2SpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample2.ston'

]

{ #category : 'tests' }
RwRowanSample2Test >> testAutomaticMigration [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after automtic migration the permanent instance variables (a-f) should retain the values, while the moved instance variables (ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMap |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

  	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: ((System gemEnvironmentVariable: 'USER') = 'dhenrich').

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: RwGsInstanceMigrator noMigration ] on: Deprecated do: [:ex | ex resume ].

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
	[ projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: RwGsInstanceMigrator aggressiveMigration ] on: Deprecated do: [:ex | ex resume ].

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
RwRowanSample2Test >> testDeferredMigration [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after deferred migration ..."

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMigrator 
		classesToMigrate expectedClassesToMigrate |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

  	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: ((System gemEnvironmentVariable: 'USER') = 'dhenrich').

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: RwGsInstanceMigrator noMigration ] on: Deprecated do: [:ex | ex resume ].

	gitTool gitcheckoutIn: gitRootPath with: 'migration_2'.
	instanceMigrator := RwGsDeferredInstanceMigrator noMigration.
	[ projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: instanceMigrator ] on: Deprecated do: [:ex | ex resume ].

	classesToMigrate := (instanceMigrator classesToMigrate collect: [:each | each name ]) sort.
	expectedClassesToMigrate := (self _migrationClassMap collect: [:each | each at: 1 ]) sort.
	self assert: classesToMigrate = expectedClassesToMigrate
]

{ #category : 'tests' }
RwRowanSample2Test >> testNoMigration [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMap |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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
RwRowanSample2Test >> testNoMigration_bitbucket [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMap |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2_bitbucketSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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
RwRowanSample2Test >> testNoMigration_gitlab [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMap |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2_gitlabSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default'  ] on: Deprecated do: [:ex | ex resume ].

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
RwRowanSample2Test >> testNoMigration_gitolite [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMap |
	((System gemEnvironmentVariable: 'USER') = 'dhenrich')
		ifFalse: [ 
			"This test only runs in the GemStone dev environment"
			^ self ].
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2_gitoliteSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

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

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample2Class subclasses rowanSampleSpec |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 6.

	gitTool gitcheckoutIn: gitRootPath with: 'migration_0'.
	[ projectTools load loadProjectNamed: projectName withConfiguration: 'Default' ] on: Deprecated do: [:ex | ex resume ].

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 0
]

{ #category : 'tests' }
RwRowanSample2Test >> testSampleDefaultConfiguration [

	"SampleDefault configuration is an instance of RwProjectConfiguration"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_0'.
	[ projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Default'
		instanceMigrator: RwGsInstanceMigrator noMigration ] on: Deprecated do: [:ex | ex resume ].

	project := RwProject newNamed: projectName.
	self assert: (x := project packageNames) = #('RowanSample2-Core')
]
