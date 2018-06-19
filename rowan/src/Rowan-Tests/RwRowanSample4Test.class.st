Class {
	#name : 'RwRowanSample4Test',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample4Test class >> _symbolDictionaryNames [

	^ 	super _symbolDictionaryNames, #(#'RowanSample4SymbolDict' #'RowanSample4DictionarySymbolDict')

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
RwRowanSample4Test >> _rowanSample4SpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample4.ston'

]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue14 [

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath theClass |
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
		useSsh: true.

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
	self assert: (x := theClass varyingConstraint) = String.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

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
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_208_0'.	

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

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
		useSsh: true.

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
		useSsh: true.

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
