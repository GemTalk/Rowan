Class {
	#name : 'RwRowanSample4Test',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample4Test class >> _symbolDictionaryNames [

	^ 	super _symbolDictionaryNames, 
			#( #'RowanSample4SymbolDict' #'RowanSample4DictionarySymbolDict' #'RowanSample4DictionarySymbolDict_295'
					#'RowanSample4DictionarySymbolDict_295_3')
]

{ #category : 'private' }
RwRowanSample4Test >> _rowanSample4_208_LoadSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample4_208_load.ston'

]

{ #category : 'private' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample4_load.ston'

]

{ #category : 'private' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl_295 [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample4_295.ston'
]

{ #category : 'private' }
RwRowanSample4Test >> _rowanSample4SpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample4.ston'

]

{ #category : 'tests' }
RwRowanSample4Test >> test_projectUrl_issue_463 [

	| specUrlString projectTools rowanSpec gitRootPath projectName rowanSampleSpec project |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	project := RwProject newNamed: projectName.
	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.

	self assert: project projectUrl = rowanSampleSpec projectUrl.
	self assert: project projectUrl = 'https://github.com/dalehenrich/RowanSample4'.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self assert: project projectUrl = 'https://github.com/dalehenrich/RowanSample4'.
]

{ #category : 'tests' }
RwRowanSample4Test >> testCreateProjectDefinition [

	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition x |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: gitRootPath, '/', projectName.

	self assert: projectDefinition packageNames isEmpty.
	
	projectTools read readProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

	self assert: (x := projectDefinition packageNames asArray sort) = #('RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests' 'RowanSample4-Tests') sort.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) isNil.
]

{ #category : 'tests' }
RwRowanSample4Test >> testCreateProjectFromUrl [

	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition spec |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	projectTools load loadProjectNamed: projectName

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue14 [

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec 
		project x repoRootPath theClass constraint |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_231_0'.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sort)
				= #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests') sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	theClass := Rowan globalNamed: 'RowanSample4'.
	self assert: (x := theClass _constraintOn: #instvar1) = Integer.
	self assert: (x := theClass _constraintOn: #instvar2) = Array.
	self assert: (x := theClass _constraintOn: #instvar3) = Boolean.
	self assert: (x := theClass _constraintOn: #instvar4) = Array.

	theClass := Rowan globalNamed: 'RowanSample4IdentityBag'.
	constraint := theClass _varyingConstraint.
	self assert: constraint = String.
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_254_move_newClassVariable_to_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_7	:: move NewRowanSample4 class with new class var to RowanSample4SymbolDict symbol dictionary (no package rename)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_7'.				"move NewRowanSample4 class with new class var to RowanSample4SymbolDict symbol dictionary (no package rename)"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass == newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_254_move_newClassVersion_to_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_5	:: move new version NewRowanSample4 class to RowanSample4SymbolDict symbol dictionary (no package rename)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_5'.				"Move new version of NewRowanSample4 class to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_254_rename_package_move_class [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_2	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move NewRowanSample4 to RowanSample4SymbolDict"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_2'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move NewRowanSample4 to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_254_rename_package_move_classVariable [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_8	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move NewRowanSample4 with new class variable to RowanSample4SymbolDict"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_8'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move NewRowanSample4 with new class variable to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_254_rename_package_move_newClassVersion [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_6	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_6'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_254_simple_package_rename [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_4	:: Simply rename RowanSample4-NewPackage to RowanSample4-RenamedPackage (no symbol dictionary move)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	self assert: (x := newClass rowanPackageName) = 'RowanSample4-NewPackage'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_4'.				"Simply rename RowanSample4-NewPackage to RowanSample4-RenamedPackage (no symbol dictionary move)"

	projectTools load
		loadProjectNamed_254: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict'.

	self assert: (x := newClass rowanPackageName) = 'RowanSample4-RenamedPackage'.

	self deny: ((Rowan globalNamed: 'RowanSample4SymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_move_class_to_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_3	:: move NewRowanSample4 class to RowanSample4SymbolDict symbol dictionary (no package rename)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_3'.				"Move NewRowanSample4 class to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_move_newClassVariable_to_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_7	:: move NewRowanSample4 class with new class var to RowanSample4SymbolDict symbol dictionary (no package rename)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_7'.				"move NewRowanSample4 class with new class var to RowanSample4SymbolDict symbol dictionary (no package rename)"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass == newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_move_newClassVersion_to_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_5	:: move new version NewRowanSample4 class to RowanSample4SymbolDict symbol dictionary (no package rename)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_5'.				"Move new version of NewRowanSample4 class to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_rename_package_move_class [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_2	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move NewRowanSample4 to RowanSample4SymbolDict"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_2'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move NewRowanSample4 to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_rename_package_move_classVariable [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_8	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move NewRowanSample4 with new class variable to RowanSample4SymbolDict"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_8'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move NewRowanSample4 with new class variable to RowanSample4SymbolDict"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_rename_package_move_newClassVersion [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_6	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_6'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue185_simple_package_rename [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_4	:: Simply rename RowanSample4-NewPackage to RowanSample4-RenamedPackage (no symbol dictionary move)"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	self assert: (x := newClass rowanPackageName) = 'RowanSample4-NewPackage'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_4'.				"Simply rename RowanSample4-NewPackage to RowanSample4-RenamedPackage (no symbol dictionary move)"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict'.

	self assert: (x := newClass rowanPackageName) = 'RowanSample4-RenamedPackage'.

	self deny: ((Rowan globalNamed: 'RowanSample4SymbolDict') includesKey: #'NewRowanSample4')

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue208_adopt_load [

	"https://github.com/dalehenrich/Rowan/issues/208"

	"Using non-Rowan api, create the classes in the appropriate symbol dictionaries, adopt into Primer project, then load branch issue_208_0 ...
		the primer project should be empty"

		"Create classes in a couple of symbol dictionaries"
		"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
		"Adopt the classes into the primer project"
		"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
		"validate that the primer project packages are empty"

	| primerProjectName specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath symDictName1 
		symDictName2 symDict theClass instanceMethod classMethod symbolList projectDefinition primerPackageName1 primerPackageName2 
		loadedPrimerProject |

	projectTools := Rowan projectTools.
	primerProjectName := 'PrimerSample4'.
	symDictName1 := 'RowanSample4SymbolDict'.
	symDictName2 := 'RowanSample4DictionarySymbolDict'.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	projectName := 'RowanSample4'.
	symbolList := Rowan image symbolList.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: 'RowanSample4'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: 'RowanSample4IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: primerProjectName.
	projectDefinition
		addPackageNamed: primerPackageName1;
		addPackageNamed: primerPackageName2;
		setSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		setSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectTools load loadProjectDefinition: projectDefinition.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4_208_LoadSpecificationUrl.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.
	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_208_0'.	

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	"validate that the primer project packages are empty"
	loadedPrimerProject := Rowan image loadedProjectNamed: primerProjectName.
	loadedPrimerProject loadedPackages 
		do: [:loadedPackage |
			self assert: loadedPackage isEmpty ].

	self assert: loadedPrimerProject isDirty.
	self deny: (Rowan image loadedProjectNamed: projectName) isDirty

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue210 [

	"https://github.com/dalehenrich/Rowan/issues/210"

	"based on testIssue208_adopt_load"

	| primerProjectName specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath symDictName1 
		symDictName2 symDict theClass instanceMethod classMethod symbolList projectDefinition primerPackageName1 primerPackageName2 |

	projectTools := Rowan projectTools.
	primerProjectName := 'PrimerSample4'.
	symDictName1 := 'RowanSample4SymbolDict'.
	symDictName2 := 'RowanSample4DictionarySymbolDict'.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	projectName := 'RowanSample4'.
	symbolList := Rowan image symbolList.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: 'RowanSample4'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: 'RowanSample4IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	"Create and load the primer project ... with two packages into which the classes will be adopted"
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: primerProjectName.
	projectDefinition
		addPackageNamed: primerPackageName1;
		addPackageNamed: primerPackageName2;
		setSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		setSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectTools load loadProjectDefinition: projectDefinition.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"now load the RowanSample4 project from disk"
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4_208_LoadSpecificationUrl.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.
	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_208_0'.	

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue230 [

	"https://github.com/dalehenrich/Rowan/issues/230"

	"new class version for class being loaded in after adopt --- initial RowanSample4 class created with instancesInvariant option, 
		so we get new version when class is loaded from disk"

	| primerProjectName specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath symDictName1 
		symDictName2 symDict theClass instanceMethod classMethod symbolList projectDefinition primerPackageName1 primerPackageName2 |

	projectTools := Rowan projectTools.
	primerProjectName := 'PrimerSample4'.
	symDictName1 := 'RowanSample4SymbolDict'.
	symDictName2 := 'RowanSample4DictionarySymbolDict'.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	projectName := 'RowanSample4'.
	symbolList := Rowan image symbolList.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: 'RowanSample4'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #(instancesInvariant).
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: 'RowanSample4IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	"Create and load the primer project ... with two packages into which the classes will be adopted"
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: primerProjectName.
	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectUnknownDefinitionSourceValue).
	projectDefinition
		addPackageNamed: primerPackageName1;
		addPackageNamed: primerPackageName2;
		setSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		setSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectTools load loadProjectDefinition: projectDefinition.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"now load the RowanSample4 project from disk"
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4_208_LoadSpecificationUrl.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.
	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_208_0'.	

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue284 [

	"https://github.com/dalehenrich/Rowan/issues/284"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath loadedCommitId |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	loadedCommitId := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId.
	self assert: loadedCommitId = '0f7683b'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_284_0'.				"README commit"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	loadedCommitId := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId.
	self assert: loadedCommitId = '8a4a450'
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_newProject_1 [

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_1 --> issue_295_2	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_2'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.
	projectTools load
		loadProjectFromSpecUrl: specUrlString
		projectRootPath: repoRootPath.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_newProject_2 [

	"attempting to reproduce the actual issue (_1 doesn't reproduce problem, but should remain static to ensure behavior does not change detrimentally"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_1 --> issue_295_3	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_3'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: repoRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

oldProjectDefinition := (Rowan image loadedProjectNamed: 'RowanSample4') asDefinition.
projectSetDefinition addProject: oldProjectDefinition.

	oldProjectDefinition packages values do: [:pkgDefinition |
	    pkgDefinition classDefinitions values do: [:classDefinition |
	        pkgDefinition removeClassDefinition: classDefinition ].
	    pkgDefinition classExtensions values do: [:classExtension |
        	pkgDefinition removeClassExtension: classExtension ]].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	Rowan projectTools load markProjectSetNotDirty: projectSetDefinition.


	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_with_subclass_newProject [

	"attempting to reproduce the actual issue (_1 doesn't reproduce problem, but should remain static to ensure behavior does not change detrimentally"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_4 --> issue_295_5	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_4'.				"New package added to the project, along with a subclass of NewRowanSample4"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_5'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: repoRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

oldProjectDefinition := (Rowan image loadedProjectNamed: 'RowanSample4') asDefinition.
projectSetDefinition addProject: oldProjectDefinition.

	oldProjectDefinition packages values do: [:pkgDefinition |
	    pkgDefinition classDefinitions values do: [:classDefinition |
	        pkgDefinition removeClassDefinition: classDefinition ].
	    pkgDefinition classExtensions values do: [:classExtension |
        	pkgDefinition removeClassExtension: classExtension ]].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	Rowan projectTools load markProjectSetNotDirty: projectSetDefinition.


	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue304 [

	"https://github.com/dalehenrich/Rowan/issues/304"

	"load a different config and group using the spec url load api ... SHA needs to change as well"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec  repoRootPath x masterBranchSHA |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.				"no tests loaded"

	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_core.ston'.

	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.

	masterBranchSHA := '66a9de3'.

	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').
	self deny: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId) = masterBranchSHA.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: masterBranchSHA.				"now load tests group"

	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_load.ston'.

	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId) = masterBranchSHA.

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue305 [

	"https://github.com/dalehenrich/Rowan/issues/305"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec  repoRootPath |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.				"useful spec --- no tests group"

	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_core.ston'.

	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue460_1 [

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_5	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass (and method) to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_6'.				"New package added to the project, along with a subclass of NewRowanSample4 with method"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_5'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: repoRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

oldProjectDefinition := (Rowan image loadedProjectNamed: 'RowanSample4') asDefinition.
projectSetDefinition addProject: oldProjectDefinition.

	oldProjectDefinition packages values do: [:pkgDefinition |
	    pkgDefinition classDefinitions values do: [:classDefinition |
	        pkgDefinition removeClassDefinition: classDefinition ].
	    pkgDefinition classExtensions values do: [:classExtension |
        	pkgDefinition removeClassExtension: classExtension ]].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	Rowan projectTools load markProjectSetNotDirty: projectSetDefinition.


	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue460_2 [

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_7	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass (and method) to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition subclass |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_6'.				"New package added to the project, along with a subclass of NewRowanSample4 with method"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_7'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: repoRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

oldProjectDefinition := (Rowan image loadedProjectNamed: 'RowanSample4') asDefinition.
projectSetDefinition addProject: oldProjectDefinition.

	oldProjectDefinition packages values do: [:pkgDefinition |
	    pkgDefinition classDefinitions values do: [:classDefinition |
	        pkgDefinition removeClassDefinition: classDefinition ].
	    pkgDefinition classExtensions values do: [:classExtension |
        	pkgDefinition removeClassExtension: classExtension ]].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	Rowan projectTools load markProjectSetNotDirty: projectSetDefinition.


	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	subclass := Rowan globalNamed: 'NewRowanSampleSubclass4'.
	self assert: subclass new foo = 'foo'.
	self assert: subclass foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue490 [

	"https://github.com/GemTalk/Rowan/issues/490"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec
		project x repoRootPath baselinePackageNames newClass ar oldClass audit |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_2'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"

	specUrlString := self _rowanSample4LoadSpecificationUrl_295.
	projectTools load
		loadProjectFromSpecUrl: specUrlString
		projectRootPath: repoRootPath.

"expose bug"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

	self assert: (((Rowan image loadedProjectNamed: 'RowanSample4')
	loadedPackages at: 'RowanSample4-Extensions')
		loadedClassExtensions isEmpty).

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_1 [

	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition spec theClass |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

	projectTools read readProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	projectTools load loadProjectNamed: projectName.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: theClass new foo = 'foo'
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_2 [

	| specUrlString projectTools rowanSpec gitRootPath projectName spec theClass |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"load project into stone"
	projectTools load loadProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: theClass new foo = 'foo'
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_300_1 [

	"https://github.com/dalehenrich/Rowan/issues/300"

	"validation for testLoadProjectFromUrl_300_1, that a non-symbolic link clone/load works"

	| specUrlString projectTools rowanSpec gitRootPath projectName spec theClass |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"load project into stone"
	projectTools load 
		loadProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'
		projectRootPath: gitRootPath, '/', projectName, '/'.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: theClass new foo = 'foo'

]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_300_2 [

	"https://github.com/dalehenrich/Rowan/issues/300"

	"regression test for bug ... mixed symbolic link and absolute path referencing same git repository"

	| specUrlString projectTools rowanSpec gitRootPath projectName spec theClass commandLine symLinkName gitRootPath_symLink |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath_symLink  := '/tmp/rowan_issue_300/'.
	gitRootPath :=  rowanSpec repositoryRootPath , '/test/testRepositories/repos/issue_300_dir/'.

	commandLine := 'set -e;  rm -rf ', gitRootPath.
	Rowan gitTools performOnServer: commandLine logging: true.

	commandLine := 'set -e;  rm -rf ', gitRootPath_symLink.
	Rowan gitTools performOnServer: commandLine logging: true.

"clone project to make sure that we have an existing git project"
	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false. 	"does not register the project, so it is not visible in project list ... does however clone the project to local disk --- which we need"

	self assert: (Rowan fileUtilities directoryExists: gitRootPath , projectName).

"create symbolic link..."
	symLinkName := 'issue_300_symLink'.
	commandLine := 'set -e;  cd ' , gitRootPath , '; mkdir ', gitRootPath_symLink, '; ln -s ', gitRootPath, ' ', gitRootPath_symLink, '/', symLinkName.
	Rowan gitTools performOnServer: commandLine logging: true.

"...and now run clone again using symbolic link" 
	spec := specUrlString asRwUrl asSpecification.
	self assert: spec repositoryUrl isNil.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath_symLink, '/', symLinkName
		useSsh: false
		registerProject: true.

	self assert: spec repositoryUrl notNil.

	"load project into stone"
	projectTools load 
		loadProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'
		projectRootPath: gitRootPath_symLink, '/', symLinkName, '/', projectName, '/'.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: theClass new foo = 'foo'.

]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_issue180 [

	"https://github.com/dalehenrich/Rowan/issues/180"

	| specUrlString projectTools rowanSpec gitRootPath projectName spec |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: false
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"load project into stone"
	projectTools load loadProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

	"load project into stone, again ... should be legal"
	projectTools load loadProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221A [

	"https://github.com/dalehenrich/Rowan/issues/221"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load core group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_core.ston'. 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').

"load tests group"
	projectTools load
		loadProjectNamed: 'RowanSample4' withGroupNames: #('tests').

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').

"load core group -- unload the tests package"
	projectTools load
		loadProjectNamed: 'RowanSample4' withGroupNames: #('core').

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').

"load Default configuration (by default) -- tests loaded"
	projectTools load
		loadProjectNamed: 'RowanSample4' withConfigurations: #() groupNames: #().

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #().
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #().

"(re)load project"
	projectTools load
		loadProjectNamed: 'RowanSample4'.

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #().
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #().

"load Load configuraiont and core group -- unload the tests package"
	projectTools load
		loadProjectNamed: 'RowanSample4' withConfigurations: #('Load') groupNames: #('core').

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221B [

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load Load_tests configuration and core group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Load' )
		groupNames: #( 'core' ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221C [

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load Load_tests configuration and tests group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Load' )
		groupNames: #( 'tests' ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221D [

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load Common configuration and empty groups ... groups do not apply to Common config"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Common' )
		groupNames: #(  ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Common').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #().
]

{ #category : 'tests' }
RwRowanSample4Test >> testSampleCompoundConfiguration [

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
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
	gitTool gitcheckoutIn: gitRootPath with: 'master'.
	projectTools load
		loadProjectNamed: projectName
		withConfiguration: 'Test'
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sort)
				= #('RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests') sort

]

{ #category : 'tests' }
RwRowanSample4Test >> testSampleProjectLoadConfiguration [

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'master'.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sort)
				= #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests') sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

]
