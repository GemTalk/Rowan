Class {
	#name : 'RwRowanSample2Test',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample2Test >> _migrationClassMap [

	^ {#(#'RowanSample2' #(#'ivar0')).
	#(#'RowanSample2A' #(#'a' #'ivar1')).
	#(#'RowanSample2C' #(#'c' #'ivar2')).
	#(#'RowanSample2D' #(#'d' #'ivar2')).
	#(#'RowanSample2B' #(#'b' #'ivar1')).
	#(#'RowanSample2E' #(#'e' #'ivar2')).
	#(#'RowanSample2F' #(#'f' #'ivar2'))}
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
			instanceMap at: instance put: ar ].

	gitTool gitcheckoutIn: gitRootPath with: 'migration_2'.
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

	instanceMap
		keysAndValuesDo: [ :instance :ar | 
			| ivNames |
			ivNames := ar at: 2.
			1 to: ivNames size do: [ :index | 
				| ivName |
				ivName := ivNames at: index.
				index == 1
					ifTrue: [ self assert: (instance perform: ivName) == ivName ]
					ifFalse: [ self assert: (instance perform: ivName) isNil ] ] ]
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
			instanceMap at: instance put: ar ].

	gitTool gitcheckoutIn: gitRootPath with: 'migration_2'.
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

	instanceMap
		keysAndValuesDo: [ :instance :ar | 
			| ivNames |
			ivNames := ar at: 2.
			1 to: ivNames size do: [ :index | 
				| ivName |
				ivName := ivNames at: index.
				(index = ivNames size or: [ index == 1 ])
					ifTrue: [ 
						"last iv slot is not niled out on migration interestingly enough"
						self assert: (instance perform: ivName) == ivName ]
					ifFalse: [ self assert: (instance perform: ivName) isNil ] ] ]
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
