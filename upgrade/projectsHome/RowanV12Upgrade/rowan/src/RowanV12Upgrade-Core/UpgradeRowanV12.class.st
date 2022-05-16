"
This class is intended to be loaded into a Rowan v1.2 image to repair and 
reload Rowan v1.2 after upgradeImage has been performed. 

The upgradeImage script removes all of the methods from kernel classes 
including the packaged Rowan extension methods. So after upgradeImage
has been run, it is necessary to reinstall the kernel class extension methods
for Rowan from a .gs file, so that Rowan is functional again.

After Rowan is functional, it is necessary to repair the damaged Rowan 
meta data for the Rowan project and any packaged extensions for 
customer application projects.

After repairing the damaged Rowan meta data, the latest version of 
Rowan v1.2 is loaded into the image, at which point the customer can
reload their application projects to restore any kernel extension methods
that may have been removed and update any other changes that they
may have made to their application code.
"
Class {
	#name : 'UpgradeRowanV12',
	#superclass : 'Object',
	#instVars : [
		'audit',
		'repairedCount',
		'skipCount',
		'repairedByReload',
		'errorMessages',
		'auditErrors',
		'projectsHome',
		'skip',
		'shouldCommit'
	],
	#category : 'RowanV12Upgrade-Core'
}

{ #category : 'Rowan V12 Upgrade' }
UpgradeRowanV12 class >> globalNamed: aString [
	"return nil if global not defined"

	^ GsSession currentSession objectNamed: aString asSymbol
]

{ #category : 'Rowan V12 Upgrade' }
UpgradeRowanV12 class >> logMessage: message [
	GsFile gciLogServer: message
]

{ #category : 'Rowan V12 Upgrade' }
UpgradeRowanV12 class >> upgradeRowan [
	"Currently only upgrades of Rowan v1.2.13 are supported"

	"During upgradeImage, all methods for kernel classes are removed, without 
		updating the Rowan metadata. So we basically need to restore the Rowan
		extension for kernel classes, repair the Rowan meta data, repair the metadata
		for the non-Rowan (customer) projects, and reload the Rowan and customer
		projects to pick up the platform-specific changes"

	"It is expected that all current Rowan projects passed a project audit prior to 
		the upgrade"

	| rowanClass upgrader |
	rowanClass := (self globalNamed: 'Rowan')
		ifNil: [ self logMessage: 'Rowan not present. No Rowan upgrade performed.' ].
	((self globalNamed: 'RwSemanticVersionNumber') fromString: rowanClass versionString)
		= ((self globalNamed: 'RwSemanticVersionNumber') fromString: '1.2.13')
		ifFalse: [ 
			self
				logMessage:
					'Upgrade for Rowan v' , rowanClass versionString
						, ' is not supported. No Rowan upgrade performed.' ].
	upgrader := self new.
	upgrader
		step_1_installRowan;
		step_2_repairRowanAuditFailures;
		step_3_repairCustomerAuditFailures;
		step_4_reloadRowan;
		step_5_reloadCustomer;
		step_6_finalAudit;
		yourself
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> auditForProjectsNamed: theProjectNames [
	audit := KeyValueDictionary new.
	theProjectNames
		do: [ :prjName | 
			audit
				addAll:
					((self globalNamed: 'Rowan') projectTools audit auditForProjectNamed: prjName) ]
]

{ #category : 'private' }
UpgradeRowanV12 >> commit [
	self shouldCommit
		ifTrue: [ System commit ]
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> countAuditErrors [
	auditErrors ifNil: [ auditErrors := 0 ].
	errorMessages := nil.
	audit
		keysAndValuesDo: [ :packageName :classAuditErrors | 
			classAuditErrors
				keysAndValuesDo: [ :className :auditAssocs | 
					auditErrors := auditErrors + auditAssocs size.
					auditAssocs do: [ :assoc | self errorMessages add: assoc value ] ] ].
	self logMessage: auditErrors printString , ' audit errors'
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> customerProjectNames [
	^ (self globalNamed: 'Rowan') projectNames
		removeAllPresent: self rowanProjectNames;
		yourself
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> customerRepairMap [
	"List of audit errors that would be expected in customer code if methods were 
		removed from classes managed by Rowan without updating the Rowan 
		metadata, as happens during upgradeImage"

	| repairMap |
	repairMap := Dictionary new.
	repairMap
		at: 'Missing instance method extension category '
			put: #'repairedWhenDefinitionsReloaded:inClassNamed:inPackageNamed:';
		at: 'Missing class method extension category '
			put: #'repairedWhenDefinitionsReloaded:inClassNamed:inPackageNamed:';
		at: 'Missing compiled class method. '
			put: #'customerRepairNonIdenticalClassMethodFor:inClassNamed:inPackageNamed:';
		at: 'Missing compiled instance method. '
			put:
				#'customerRepairNonIdenticalInstanceMethodFor:inClassNamed:inPackageNamed:';
		at: 'Compiled instance method is not identical to loaded instance method. '
			put:
				#'customerRepairNonIdenticalInstanceMethodFor:inClassNamed:inPackageNamed:';
		at: 'Compiled instance method is not identical to loaded method. '
			put:
				#'customerRepairNonIdenticalInstanceMethodFor:inClassNamed:inPackageNamed:';
		at: 'Compiled classmethod is not identical to loaded class method '
			put: #'customerRepairNonIdenticalClassMethodFor:inClassNamed:inPackageNamed:';
		at: 'Comment has changed in compiled class v loaded class'
			put: #'repairedWhenDefinitionsReloaded:inClassNamed:inPackageNamed:';
		yourself.
	^ repairMap
]

{ #category : 'repair' }
UpgradeRowanV12 >> customerRepairNonIdenticalClassMethodFor: methodSpec inClassNamed: className inPackageNamed: packageName [
	^ self customerRepairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: true inPackageNamed: packageName

]

{ #category : 'repair' }
UpgradeRowanV12 >> customerRepairNonIdenticalInstanceMethodFor: methodSpec inClassNamed: className inPackageNamed: packageName [
	^ self customerRepairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: false inPackageNamed: packageName

]

{ #category : 'repair' }
UpgradeRowanV12 >> customerRepairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	| loadedMethod loadedClass loadedPackage loadedProject selector theClass theBehavior oldCompiledMethod
		registryInstance newCompiledMethod |

	loadedPackage := (self globalNamed: 'Rowan') image loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ self error: 'No loaded class or loaded extension class found for ', className printString ].
	selector := self selectorFromMethodSpec: methodSpec.
	loadedMethod := isMeta
		ifTrue: [ loadedClass loadedClassMethods at: selector ]
		ifFalse: [ loadedClass loadedInstanceMethods at: selector ].
	loadedProject := loadedMethod loadedProject.
	theClass := self globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	newCompiledMethod := theBehavior compiledMethodAt: selector otherwise: nil.
	oldCompiledMethod := loadedMethod handle.

	newCompiledMethod == oldCompiledMethod
		ifTrue: [
			"confirm audit error"
			self error: 'identical compiled methods when non-identical compiled methods expected for ', className, (isMeta ifTrue: [ ' class >> ' ] ifFalse: [' >> ']), selector printString ].

	"For customer repair, we ignore the newCompiledMethod (if it exists), since the incoming
		method will be installed using Rowan"

	registryInstance := (self globalNamed: 'Rowan') image loadedRegistryForPackageNamed: packageName.
	(self globalNamed: 'RwGsSymbolDictionaryRegistry_Implementation')
		_doDeleteCompiledMethodFromLoadedThings: oldCompiledMethod
		for: theBehavior
		instance: registryInstance.

	repairedCount := self repairedCount + 1.
]

{ #category : 'private' }
UpgradeRowanV12 >> cypressClassNames [
	"Rowan v1.2.13 Cypress class names"

	^ {'CypressLoaderErrorNotification'.
	'CypressFileSystemGitRepository'.
	'CypressUnknownPackageInformation'.
	'CypressAbstractPackageFiler'.
	'CypressFileUtilities'.
	'CypressFlexiblePackageReader'.
	'CypressHierarchicalUrl'.
	'CypressSnapshot'.
	'CypressHttpsUrl'.
	'CypressDictionaryRepository'.
	'CypressPackageStringComparator'.
	'CypressLoaderError'.
	'CypressObject'.
	'CypressConflictingPackageInformation'.
	'CypressFileTreeFormatPackageReader'.
	'CypressPatch'.
	'CypressTopazUrl'.
	'CypressSmalltalkUrl'.
	'CypressEnvironmentPackageManager'.
	'CypressBrowserUrl'.
	'CypressMailtoUrl'.
	'CypressJsonParser'.
	'CypressStructure'.
	'CypressPackageWriter'.
	'CypressAddition'.
	'CypressVersionReference'.
	'CypressGsGeneralDependencySorter'.
	'CypressUrl'.
	'CypressTopazFileoutWriter'.
	'CypressError'.
	'CypressFileTreeFormatPackageWriter'.
	'CypressFileUrl'.
	'CypressPackageComparator'.
	'CypressStrictFileTreeFormatDoNothingPackageWriter'.
	'CypressGitFileUrl'.
	'CypressAbstractFileoutWriter'.
	'CypressPackageManager'.
	'CypressAbstractFileoutRepository'.
	'CypressAbstractRepository'.
	'CypressGemStoneDirectoryUtilities'.
	'CypressDefinition'.
	'CypressKnownPackageInformation'.
	'CypressDefinitionIndex'.
	'CypressResolvedReference'.
	'CypressSmalltalkFileoutWriter'.
	'CypressFileSystemRepository'.
	'CypressCypressFileUrl'.
	'CypressClassStructure'.
	'CypressJsonError'.
	'CypressFileTreeFormatFileUrl'.
	'CypressEnvironmentDependencySorter'.
	'CypressReference'.
	'CypressFileTreeReadOnlyFileUrl'.
	'CypressModification'.
	'CypressMessageDigestStream'.
	'CypressSmalltalkRepository'.
	'CypressEnvironmentLoader'.
	'CypressHttpUrl'.
	'CypressPackageManager3'.
	'CypressPackageReader'.
	'CypressFtpUrl'.
	'CypressPackageManager2'.
	'CypressLoaderMissingClasses'.
	'CypressRemoval'.
	'CypressPackageStructure'.
	'CypressLaxFileUrl'.
	'CypressMethodStructure'.
	'CypressGenericUrl'.
	'CypressPackageDefinition'.
	'CypressPackageReference'.
	'CypressMethodDefinition'.
	'CypressDoNothingPackageReader'.
	'CypressClassDefinition'.
	'CypressPatchOperation'.
	'CypressAbstractFileUrl'.
	'CypressAbstractPackageWriter'.
	'CypressGitFileTreeUrl'.
	'CypressPackageInformation'.
	'CypressLoader'.
	'CypressAbstractPackageInformation'.
	'CypressAbstractPackageReader'.
	'CypressEnvironmentPackageDefinition'.
	'CypressEclipsedPackageInformation'.
	'CypressTopazRepository'.
	'CypressDependencySorter'}
]

{ #category : 'accessing' }
UpgradeRowanV12 >> errorMessages [
^ errorMessages ifNil: [ errorMessages := Bag new ].

]

{ #category : 'accessing' }
UpgradeRowanV12 >> gemstoneVersion [
	"run during upgrade, so CharacterCollection>>asRwGemStoneVersionNumber may not be functional"

	^ RwGemStoneVersionNumber fromString: self gsVersion
]

{ #category : 'private' }
UpgradeRowanV12 >> globalNamed: aString [
	"return nil if global not defined"

	^ self class globalNamed: aString
]

{ #category : 'accessing' }
UpgradeRowanV12 >> gsVersion [
	^ System gemVersionReport at: 'gsVersion'
]

{ #category : 'private' }
UpgradeRowanV12 >> logMessage: message [
	self class logMessage: message
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> moveCypressClassesToGlobals: cypressClassNames [
	"upgradeImage moves Cypress* classes in Globals to ObsoleteClasses
		(associated with upgrade from 3.4.x), so we need to restore the 
		Cypress classes currently used by v1.2.x"

	cypressClassNames
		do: [ :name | 
			(ObsoleteClasses associationAt: name asSymbol ifAbsent: [  ])
				ifNotNil: [ :assoc | Globals addAssociation: assoc ] ]
]

{ #category : 'accessing' }
UpgradeRowanV12 >> projectsHome [
	^ projectsHome ifNil: [ projectsHome := '$ROWAN_PROJECTS_HOME' ]
]

{ #category : 'accessing' }
UpgradeRowanV12 >> projectsHome: aPathString [
	projectsHome := aPathString
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> reloadCustomer [
	| customerProjectNames |
	customerProjectNames := self customerProjectNames.
	customerProjectNames
		do: [ :projectName | 
			self logMessage: 'Loading ' , projectName.
			(self globalNamed: 'Rowan') projectTools load loadProjectNamed: projectName ]
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> reloadRowan [
	self rowanProjectNames
		do: [:projectName |
			"make sure test group is loaded ... include deprecated packages"
			[
			| groupNames |
			groupNames := #('tests' 'deprecated' 'jadeServer').
			self logMessage: 'Loading ', projectName.
			(self globalNamed: 'Rowan') projectTools load
				loadProjectNamed: projectName
				withGroupNames: groupNames ]
					on: CompileWarning do: [:ex | ex resume ] ].
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> repairAuditFailures: repairMap [
	errorMessages := nil.
	audit
		keysAndValuesDo: [ :packageName :classAuditErrors | 
			| method |
			classAuditErrors
				keysAndValuesDo: [ :className :auditAssocs | 
					auditAssocs
						do: [ :assoc | 
							| error repairSelector |
							method := assoc key.
							error := assoc value.
							self errorMessages add: error.
							repairSelector := repairMap
								at: error
								ifAbsent: [ 
									self skip
										ifTrue: [ #'skipRepair:inClassNamed:inPackageNamed:' ]
										ifFalse: [ self error: 'unrepairable audit error: ' , error printString ] ].
							self
								perform: repairSelector
								with: method
								with: className
								with: packageName ] ] ].
	self repairSummary
]

{ #category : 'accessing' }
UpgradeRowanV12 >> repairedByReload [
	^ repairedByReload ifNil: [ repairedByReload := 0 ].

]

{ #category : 'accessing' }
UpgradeRowanV12 >> repairedCount [
	^ repairedCount ifNil: [ repairedCount := 0 ].

]

{ #category : 'repair' }
UpgradeRowanV12 >> repairedWhenDefinitionsReloaded:ignoredMethod inClassNamed: ignoredClassName inPackageNamed: ignoredPackageName [
	repairedByReload := self repairedByReload + 1

]

{ #category : 'repair' }
UpgradeRowanV12 >> repairMissingLoadedClassMethodFor: methodSpec inClassNamed: className inPackageNamed: packageName [
	^ self repairMissingLoadedMethodFor: methodSpec inClassNamed: className isMeta: true inPackageNamed: packageName
]

{ #category : 'repair' }
UpgradeRowanV12 >> repairMissingLoadedInstanceMethodFor: methodSpec inClassNamed: className inPackageNamed: packageName [
	^ self repairMissingLoadedMethodFor: methodSpec inClassNamed: className isMeta: false inPackageNamed: packageName
]

{ #category : 'repair' }
UpgradeRowanV12 >> repairMissingLoadedMethodFor: methodSpec inClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	| loadedMethod loadedClass loadedPackage loadedProject selector theClass theBehavior 
		newCompiledMethod registryInstance existing |

	self
		logMessage:
			'  Repairing missing loaded method...'.

	loadedPackage := (self globalNamed: 'Rowan') image loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ self error: 'No loaded class or loaded extension class found for ', className printString ].
	selector := self selectorFromMethodSpec: methodSpec.
	loadedProject := loadedClass loadedProject.
	theClass := self globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	newCompiledMethod := theBehavior compiledMethodAt: selector.

"create new loaded method"
	registryInstance := (self globalNamed: 'Rowan') image loadedRegistryForPackageNamed: packageName.
	existing := registryInstance methodRegistry at: newCompiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ registryInstance error: 'Internal error -- existing LoadedMethod found for compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: newCompiledMethod.
	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod.
	loadedClass addLoadedMethod: loadedMethod.

	self
		logMessage:
			'  Repair missing loaded method: '
				, theBehavior printString , '>>' , selector , ' for package '
				, packageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'repair' }
UpgradeRowanV12 >> repairNonIdenticalClassMethodFor: methodSpec inClassNamed: className inPackageNamed: packageName [
	^ self repairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: true inPackageNamed: packageName

]

{ #category : 'repair' }
UpgradeRowanV12 >> repairNonIdenticalInstanceMethodFor: methodSpec inClassNamed: className inPackageNamed: packageName [
	^ self repairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: false inPackageNamed: packageName

]

{ #category : 'repair' }
UpgradeRowanV12 >> repairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	| loadedMethod loadedClass loadedPackage loadedProject selector theClass theBehavior oldCompiledMethod
		newCompiledMethod registryInstance |

	loadedPackage := (self globalNamed: 'Rowan') image loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ self error: 'No loaded class or loaded extension class found for ', className printString ].
	selector := self selectorFromMethodSpec: methodSpec.
	loadedMethod := isMeta
		ifTrue: [ loadedClass loadedClassMethods at: selector ]
		ifFalse: [ loadedClass loadedInstanceMethods at: selector ].
	loadedProject := loadedMethod loadedProject.
	theClass := self globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	newCompiledMethod := theBehavior compiledMethodAt: selector.
	oldCompiledMethod := loadedMethod handle.

	newCompiledMethod == oldCompiledMethod
		ifTrue: [
			"confirm audit error"
			self error: 'identical compiled methods when non-identical compiled methods expected for ', className, (isMeta ifTrue: [ ' class >> ' ] ifFalse: [' >> ']), selector printString ].

	registryInstance := (self globalNamed: 'Rowan') image loadedRegistryForPackageNamed: packageName.
	registryInstance methodRegistry removeKey: oldCompiledMethod.
	loadedMethod handle: newCompiledMethod.
	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod.
	self
		logMessage:
			'  Repair nonidentical method: '
				, theBehavior printString , '>>' , selector , ' for package '
				, packageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> repairSummary [
	self logMessage: 'Audit errors encountered:'.
	self errorMessages asSet asArray
		do: [ :message | 
			self
				logMessage:
					'	' , message , '(seen ' , (errorMessages occurrencesOf: message) printString
						, ' times)' ].
	self logMessage: '---'.
	self
		logMessage: 'Repaired ' , self repairedCount printString , ' audit failures'.
	self
		logMessage:
			'Repaired ' , self repairedByReload printString , ' by RELOAD audit failures'.
	self logMessage: 'Skipped ' , self skipCount printString , ' audit repairs'
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> rowanProjectNames [
	^ #('Rowan' 'STON' 'Cypress' 'Tonel')
]

{ #category : 'upgrade' }
UpgradeRowanV12 >> rowanRepairMap [
	"List of audit errors that would be expected in Rowan code if methods were 
		removed from classes managed by Rowan without updating the Rowan 
		metadata, as happens during upgradeImage"

	| repairMap |
	repairMap := Dictionary new.
	repairMap
		at: 'Compiled instance method is not identical to loaded method. '
			put: #'repairNonIdenticalInstanceMethodFor:inClassNamed:inPackageNamed:';
		at: 'Compiled classmethod is not identical to loaded class method '
			put: #'repairNonIdenticalClassMethodFor:inClassNamed:inPackageNamed:';
		at: 'Comment has changed in compiled class v loaded class'
			put: #'repairedWhenDefinitionsReloaded:inClassNamed:inPackageNamed:';
		at: 'Missing loaded instance method. ' put: #'repairMissingLoadedInstanceMethodFor:inClassNamed:inPackageNamed:';
		yourself.
	^ repairMap
]

{ #category : 'private' }
UpgradeRowanV12 >> selectorFromMethodSpec: methodSpec [
	| index |
	index := methodSpec indexOfSubCollection: ' >> ' startingAt: 1.
	^ (methodSpec copyFrom: index + 4 to: methodSpec size) asSymbol

]

{ #category : 'accessing' }
UpgradeRowanV12 >> shouldCommit [
	^ shouldCommit ifNil: [ shouldCommit := false ]
]

{ #category : 'accessing' }
UpgradeRowanV12 >> shouldCommit: object [
	shouldCommit := object
]

{ #category : 'accessing' }
UpgradeRowanV12 >> skip [
	^ skip ifNil: [ skip := true ]
]

{ #category : 'accessing' }
UpgradeRowanV12 >> skipCount [
	^ skipCount ifNil: [ skipCount := 0 ]

]

{ #category : 'repair' }
UpgradeRowanV12 >> skipRepair: ignoredMethod inClassNamed: ignoredClassName inPackageNamed: ignoredPackageName [
	skipCount := self skipCount + 1

]

{ #category : 'steps' }
UpgradeRowanV12 >> step_1_installRowan [
	"upgradeImage moves Cypress classes from Globals to ObsoleteClasses so they
		need to be restored and Rowan needs to be installed from a .gs file, which 
		corrupts the Rowan metadata, but gives a functional Rowan implementation 
		that will then be used to repair the corrupted metadata."

	| rowanBootstrapPath |
	self
		logMessage:
			'Rowan v' , (self globalNamed: 'Rowan') versionString
				, ' currently installed in GemStone' , (System stoneVersionAt: 'gsVersion').
	self
		logMessage: 'Moving active Cypress classes from ObsoleteClasses to Globals'.
	self moveCypressClassesToGlobals: self cypressClassNames.
	self logMessage: 'Installing RowanV12.gs'.
	rowanBootstrapPath := self projectsHome
		, '/Rowan/platforms/gemstone/topaz/upgrade/' , self gsVersion , '/RowanV12.gs'.
	(self globalNamed: 'GsFileIn') fromServerPath: rowanBootstrapPath.
	self logMessage: 'Installed Rowan from ' , rowanBootstrapPath.
	self commit
]

{ #category : 'steps' }
UpgradeRowanV12 >> step_2_repairRowanAuditFailures [
	"Use the Rowan project audit to repair the damaged Rowan metadata"

	self logMessage: ' repair ROWAN audit failures'.
	self auditForProjectsNamed: self rowanProjectNames.
	audit isEmpty
		ifFalse: [ 
			self repairAuditFailures: self rowanRepairMap.
			self commit ]
]

{ #category : 'steps' }
UpgradeRowanV12 >> step_3_repairCustomerAuditFailures [
	"Use the Rowan project audit to repair any damaged customer metadata, caused by the removal of base extension methods"

	self logMessage: ' repair CUSTOMER audit failures'.
	self auditForProjectsNamed: self customerProjectNames.
	audit isEmpty
		ifFalse: [ 
			self repairAuditFailures: self customerRepairMap.
			self commit ]
]

{ #category : 'steps' }
UpgradeRowanV12 >> step_4_reloadRowan [
	"Reload the Rowan project to repair those audit failures repaired by a reload"

	self logMessage: 'reload Rowan'.
	self reloadRowan.
	self commit
]

{ #category : 'steps' }
UpgradeRowanV12 >> step_5_reloadCustomer [
	"Reload the Customer projects to repair those audit failures repaired by a reload and to install platform-specific changes"

	self logMessage: 'reload Customer projects'.
	self reloadCustomer.
	self commit
]

{ #category : 'steps' }
UpgradeRowanV12 >> step_6_finalAudit [
	self logMessage: 'Final Audit'.
	self auditForProjectsNamed: (self globalNamed: 'Rowan') projectNames.
	self countAuditErrors.
	self logMessage: 'Audit errors encountered:'.
	self errorMessages asSet asArray
		do: [ :message | 
			self
				logMessage:
					'	' , message , '(seen ' , (errorMessages occurrencesOf: message) printString
						, ' times)' ].
	audit isEmpty
		ifFalse: [ 
			self
				error:
					'Final Audit did not run clean ... repair audit errors before proceeding' ]
]
