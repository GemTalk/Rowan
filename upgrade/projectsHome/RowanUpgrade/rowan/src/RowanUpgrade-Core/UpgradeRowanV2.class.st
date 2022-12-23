"
This class is intended to be loaded into a Rowan v2.2 image to repair and 
reload Rowan v2.2 after upgradeImage has been performed. 

The upgradeImage script removes all of the methods from kernel classes 
including the packaged Rowan extension methods. So after upgradeImage
has been run, it is necessary to reinstall the kernel class extension methods
for Rowan from a .gs file, so that Rowan is functional again.

After Rowan is functional, it is necessary to repair the damaged Rowan 
meta data for the Rowan project and any packaged extensions for 
customer application projects.

After repairing the damaged Rowan meta data, the latest version of 
Rowan v2.2 (masterV2.2 branch) is loaded into the image, at which point
the customer can reload their application projects to restore any kernel 
extension methods that may have been removed and update any other 
changes that they may have made to their application code.
"
Class {
	#name : 'UpgradeRowanV2',
	#superclass : 'Object',
	#instVars : [
		'upgradeFrom',
		'audit',
		'missingLoadedNotIdenticalCandidateMap',
		'repairedCount',
		'skipCount',
		'repairedByReload',
		'errorMessages',
		'auditErrors',
		'projectsHome',
		'skip',
		'shouldCommit'
	],
	#category : 'RowanUpgrade-Core'
}

{ #category : 'Rowan Upgrade' }
UpgradeRowanV2 class >> globalNamed: aString [
	"return nil if global not defined"

	^ GsSession currentSession objectNamed: aString asSymbol
]

{ #category : 'Rowan Upgrade' }
UpgradeRowanV2 class >> logMessage: message [
	GsFile gciLogServer: message
]

{ #category : 'private' }
UpgradeRowanV2 >> _analyzeAuditForMissingLoadedNotIdentical [
	"
		When we find the pattern of a pair of auditDetails for the same package, behavior 
		and selector, where one reason is #'missingLoadedMethodDetail' and the other 
		reason is #'methodsNotIdenticalDetail', they need to be repaired at the same 
		time using the method #repairMissingLoadedNotIdentical:forPackageName:andClassName:
	"

	missingLoadedNotIdenticalCandidateMap := IdentityKeyValueDictionary new.
	audit
		keysAndValuesDo: [ :packageName :auditReport | 
			auditReport
				keysAndValuesDo: [ :theClassName :auditDetails | 
					| methodsNotIdenticalMap |
					methodsNotIdenticalMap := IdentityKeyValueDictionary new.
					(auditDetails
						select: [ :auditDetail | auditDetail reason == #'methodsNotIdentical' ])
						do: [ :auditDetail | 
							(methodsNotIdenticalMap
								at: auditDetail behavior
								ifAbsentPut: [ SymbolKeyValueDictionary new ])
								at: auditDetail selector
								put: auditDetail ].
					(auditDetails
						select: [ :auditDetail | auditDetail reason == #'missingLoadedMethod' ])
						do: [ :auditDetail | 
							((methodsNotIdenticalMap includesKey: auditDetail behavior)
								and: [ 
									(methodsNotIdenticalMap at: auditDetail behavior)
										includesKey: auditDetail selector ])
								ifTrue: [ 
									((missingLoadedNotIdenticalCandidateMap
										at: auditDetail behavior
										ifAbsentPut: [ SymbolKeyValueDictionary new ])
										at: auditDetail selector
										ifAbsentPut: [ Dictionary new ])
										at: #'missingLoadedMethodDetail' put: auditDetail;
										at: #'methodsNotIdenticalDetail'
											put:
												((methodsNotIdenticalMap at: auditDetail behavior) at: auditDetail selector) ] ] ] ]
]

{ #category : 'upgrade gemstoneBaseImage' }
UpgradeRowanV2 >> adoptGemstoneBase [
	"classes that are new relative to the original GemStone version, need to be adopted prior to release"

	self gemstoneVersion >= '3.6.4' asRwGemStoneVersionNumber
		ifTrue: [ 
			| gciLibraryClass |
			"GciLibrary is a class that does not have managed source, as it is 
				generated and loaded on the fly. 
			The UnPackagedProject is a temporary project for classes that 
				do not have managed source."
			gciLibraryClass := Rowan globalNamed: 'GciLibrary'.
			Rowan packageTools disown disownClass: gciLibraryClass.
			Rowan packageTools adopt
				adoptClass: gciLibraryClass
				intoPackageNamed: 'UnPackaged-Globals' ].
	(self gemstoneVersion >= '3.6.4' asRwGemStoneVersionNumber
		and: [ self upgradeFrom asRwGemStoneVersionNumber < '3.6.4' asRwGemStoneVersionNumber ])
		ifTrue: [ 
			"CCalloutStructs is a new class in 3.6.4 that needs to be adopted 
				before reloading GemStone packages."
			Rowan packageTools adopt
				adoptClassNamed: 'CCalloutStructs'
				instanceSelectors: #()
				classSelectors: #()
				intoPackageNamed: 'Filein4Rowan' ].
	(self gemstoneVersion >= '3.6.3' asRwGemStoneVersionNumber
		and: [ self upgradeFrom asRwGemStoneVersionNumber < '3.6.3' asRwGemStoneVersionNumber ])
		ifTrue: [ 
			"AwsCredentials, AwsDataKey,  AwsError are new classes in 3.6.3 that need 
				to be adopted (along with all of their methods) before reloading 
				GemStone packages."
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'AwsCredentials')
				intoPackageNamed: 'Filein3B'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'AwsDataKey')
				intoPackageNamed: 'Filein3B'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'AwsError')
				intoPackageNamed: 'Filein3B' ].
	(self gemstoneVersion >= '3.7.0' asRwGemStoneVersionNumber
		and: [ self upgradeFrom asRwGemStoneVersionNumber <= '3.6.3' asRwGemStoneVersionNumber ])
		ifTrue: [ 
			"old version of GsHostProcess made obsolete and a new version of GsHostProcess added"
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'GsHostProcess')
				intoPackageNamed: 'Filein2A' ].
	(self gemstoneVersion >= '3.7.0' asRwGemStoneVersionNumber
		and: [ self upgradeFrom asRwGemStoneVersionNumber < '3.7.0' asRwGemStoneVersionNumber ])
		ifTrue: [ 
			"GcFinalizeNotification, GsSftpRemoteFile, GsSftpSocket, GsSshSocket, JsonParser,
				ReadByteStream, SshSocketError are new classes in 3.7.0 that need 
				to be adopted (along with all of their methods) before reloading 
				GemStone packages."
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'GcFinalizeNotification')
				intoPackageNamed: 'Filein4Rowan'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'GsSftpRemoteFile')
				intoPackageNamed: 'Filein3B'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'GsSftpSocket')
				intoPackageNamed: 'Filein3B'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'GsSshSocket')
				intoPackageNamed: 'Filein3B'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'JsonParser')
				intoPackageNamed: 'Filein3B'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'ReadByteStream')
				intoPackageNamed: 'UnPackaged-Globals'.
			Rowan packageTools adopt
				adoptClass: (Rowan globalNamed: 'SshSocketError')
				intoPackageNamed: 'Filein3B' ]
]

{ #category : 'upgrade' }
UpgradeRowanV2 >> auditForProjectsNamed: theProjectNames [
	audit := KeyValueDictionary new.
	theProjectNames
		do: [ :prjName | 
			audit
				addAll:
					((self globalNamed: 'Rowan') projectTools audit auditForProjectNamed: prjName) ].
	self _analyzeAuditForMissingLoadedNotIdentical
]

{ #category : 'private' }
UpgradeRowanV2 >> commit [
	self shouldCommit
		ifTrue: [ System commit ]
]

{ #category : 'upgrade' }
UpgradeRowanV2 >> countAuditErrors [
	auditErrors ifNil: [ auditErrors := 0 ].
	errorMessages := nil.
	audit
		keysAndValuesDo: [ :packageName :classAuditErrors | 
			classAuditErrors
				keysAndValuesDo: [ :className :auditAssocs | 
					auditErrors := auditErrors + auditAssocs size.
					auditAssocs do: [ :assoc | self errorMessages add: assoc value message ] ] ].
	self logMessage: auditErrors printString , ' audit errors'
]

{ #category : 'upgrade customer' }
UpgradeRowanV2 >> customerProjectNames [
	^ (self globalNamed: 'Rowan') projectNames
		removeAllPresent: self rowanProjectNames;
		yourself
]

{ #category : 'repair' }
UpgradeRowanV2 >> customerRepairDifferentMethodCategory: auditDetail [
	"different method categories are repaired by moving the method to the expected category"

	self
		repairDifferentMethodCategory: auditDetail
]

{ #category : 'upgrade customer' }
UpgradeRowanV2 >> customerRepairMap [
	"List of audit errors that would be expected in customer code if methods were 
		removed from classes managed by Rowan without updating the Rowan 
		metadata, as happens during upgradeImage"

	| repairMap |
	repairMap := Dictionary new.
	repairMap
		at: #'differentMethodCategory' put: #'customerRepairDifferentMethodCategory:';
		at: #'missingExtensionCategory' put: #'repairedWhenDefinitionsReloaded:';
		at: #'missingCompiledMethod' put: #'customerRepairMissingCompiledMethod:';
		at: #'missingCompiledMethodsForLoadedClassExtension'
			put: #'customerRepairMissingCompiledMethod:';
		at: #'missingLoadedMethod' put: #'customerRepairMissingLoadedMethod:';
		at: #'methodsNotIdentical' put: #'customerRepairNonIdenticalMethod:';
		at: #'differentComment' put: #'repairedWhenDefinitionsReloaded:';
		yourself.
	^ repairMap
]

{ #category : 'repair' }
UpgradeRowanV2 >> customerRepairMissingCompiledMethod: auditDetail [
	"missing compiled methods (because upgradeImage has removed them) can be repaired by 
		removing the associated loaded method, which will allow the method to be loaded 
		into the image, when project is reloaded"

	self
		repairMissingCompiledMethod:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> customerRepairMissingLoadedMethod: auditDetail [
	"missing loaded methods are a special case of missing compiled method, 
		where the compiled method was initially removed and then replaced 
		by a new compiled method thus losing the reference the loaded method, 
		can be repaired by splicing the loaded method into the new compiled method"

	self
		repairMissingLoadedMethod:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedClassOrClassExtension loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> customerRepairNonIdenticalMethod: auditDetail [
	"nonIdentical methods (because upgradeImage has removed and recompiled the 
		compiled method, leaving the loaded method referencing a dangling compiled 
		method reference) can be repaired by slicing the new compiled method into 
		the loaded method. The method to be loaded into the image, if the source of
		the new method doesn't match the source of the incoming method."

	self
		repairNonIdenticalMethodFor:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'private' }
UpgradeRowanV2 >> cypressClassNames [
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
UpgradeRowanV2 >> errorMessages [
^ errorMessages ifNil: [ errorMessages := Bag new ].

]

{ #category : 'upgrade gemstoneBaseImage' }
UpgradeRowanV2 >> gemstoneBaseProjectNames [
	^ #( 'gemstoneBaseImage' 'FileSystemGs' )
]

{ #category : 'repair' }
UpgradeRowanV2 >> gemstoneBaseRepairClassesNotIdentical: auditDetail [
	"classes not identical when the loadedClass is referencing an older version of class whose shape 
		changed during upgradeImage of GemStone, can be repaired by adopting the class into a 
		package ... and when the definition for the new version of the class is loaded the class will be 
		moved to the correct package"

	self
		repairClassesNotIdentical: auditDetail loadedClass name
		inPackageNamed: auditDetail loadedClass loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> gemstoneBaseRepairDifferentMethodCategory: auditDetail [
	"different method categories are repaired by moving the method to the expected category"

	self
		repairDifferentMethodCategory: auditDetail
]

{ #category : 'upgrade gemstoneBaseImage' }
UpgradeRowanV2 >> gemstoneBaseRepairMap [
	"List of audit errors that would be expected in gemsteBaseImage code if methods were 
		removed from classes managed by Rowan without updating the Rowan 
		metadata, as happens during upgradeImage"

	| repairMap |
	repairMap := Dictionary new.
	repairMap
		at: #'classesNotIdentical' put: #'gemstoneBaseRepairClassesNotIdentical:';
		at: #'differentMethodCategory'
			put: #'gemstoneBaseRepairDifferentMethodCategory:';
		at: #'missingExtensionCategory' put: #'repairedWhenDefinitionsReloaded:';
		at: #'missingCompiledMethod' put: #'gemstoneBaseRepairMissingCompiledMethod:';
		at: #'missingCompiledMethodsForLoadedClassExtension'
			put: #'gemstoneBaseRepairMissingCompiledMethod:';
		at: #'missingLoadedMethod' put: #'gemstoneBaseRepairMissingLoadedMethod:';
		at: #'methodsNotIdentical' put: #'gemstoneBaseRepairNonIdenticalMethod:';
		at: #'differentClassInstVars' put: #'repairedWhenDefinitionsReloaded:';
		at: #'differentComment' put: #'repairedWhenDefinitionsReloaded:';
		at: #'loadedPackageNotInRegistry' put: #'repairLoadedPackageNotInRegistry:';
		yourself.
	^ repairMap
]

{ #category : 'repair' }
UpgradeRowanV2 >> gemstoneBaseRepairMissingCompiledMethod: auditDetail [
	"missing compiled methods (because upgradeImage has removed them) can be repaired by 
		removing the associated loaded method, which will allow the method to be loaded 
		into the image, when project is reloaded"

	self
		repairMissingCompiledMethod:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> gemstoneBaseRepairMissingLoadedMethod: auditDetail [
	"missing loaded methods are a special case of missing compiled method, 
		where the compiled method was initially removed and then replaced 
		by a new compiled method thus losing the reference the loaded method, 
		can be repaired by splicing the loaded method into the new compiled method"

	self
		repairMissingLoadedMethod:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedClassOrClassExtension loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> gemstoneBaseRepairNonIdenticalMethod: auditDetail [
	"nonIdentical methods (because upgradeImage has removed and recompiled the 
		compiled method, leaving the loaded method referencing a dangling compiled 
		method reference) can be repaired by slicing the new compiled method into 
		the loaded method. The method to be loaded into the image, if the source of
		the new method doesn't match the source of the incoming method."

	self
		repairNonIdenticalMethodFor:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'accessing' }
UpgradeRowanV2 >> gemstoneVersion [
	^ self gsVersion asRwGemStoneVersionNumber
]

{ #category : 'private' }
UpgradeRowanV2 >> globalNamed: aString [
	"return nil if global not defined"

	^ self class globalNamed: aString
]

{ #category : 'accessing' }
UpgradeRowanV2 >> gsVersion [
	^ System gemVersionReport at: 'gsVersion'
]

{ #category : 'private' }
UpgradeRowanV2 >> logMessage: message [
	self class logMessage: message
]

{ #category : 'upgrade' }
UpgradeRowanV2 >> moveCypressClassesToGlobals: cypressClassNames [
	"upgradeImage moves Cypress* classes in Globals to ObsoleteClasses
		(associated with upgrade from 3.4.x), so we need to restore the 
		Cypress classes currently used by v1.2.x"

	cypressClassNames
		do: [ :name | 
			(ObsoleteClasses associationAt: name asSymbol ifAbsent: [  ])
				ifNotNil: [ :assoc | Globals addAssociation: assoc ] ]
]

{ #category : 'accessing' }
UpgradeRowanV2 >> projectsHome [
	^ projectsHome ifNil: [ projectsHome := '$ROWAN_PROJECTS_HOME' ]
]

{ #category : 'accessing' }
UpgradeRowanV2 >> projectsHome: aPathString [
	projectsHome := aPathString
]

{ #category : 'upgrade customer' }
UpgradeRowanV2 >> reloadCustomer [
	| customerProjectNames |
	customerProjectNames := self customerProjectNames reject: [:projectName | projectName = 'UnPackaged' ].
	customerProjectNames
		do: [ :projectName | 
			self logMessage: 'Loading ' , projectName.
			(self globalNamed: 'Rowan') projectTools load loadProjectNamed: projectName ]
]

{ #category : 'upgrade gemstoneBaseImage' }
UpgradeRowanV2 >> reloadGemstoneBase [
	self adoptGemstoneBase.
	self gemstoneBaseProjectNames
		do: [ :projectName | 
			"make sure test group is loaded ... include deprecated packages"
			[ (Rowan projectNamed: projectName) reload ]
				on: CompileWarning
				do: [ :ex | ex resume ] ]
]

{ #category : 'upgrade rowan' }
UpgradeRowanV2 >> reloadRowan [
	self rowanProjectNames
		do: [ :projectName | 
			"make sure test group is loaded ... include deprecated packages"
			[ (Rowan projectNamed: projectName) reload ]
				on: CompileWarning
				do: [ :ex | ex resume ] ]
]

{ #category : 'upgrade' }
UpgradeRowanV2 >> repairAuditFailures: repairMap [
	errorMessages := nil.
	audit
		keysAndValuesDo: [ :packageName :auditReport | 
			| reason |
			auditReport
				keysAndValuesDo: [ :className :auditDetails | 
					| repairedDetails |
					repairedDetails := IdentitySet new.
					auditDetails
						do: [ :auditDetail | 
							(repairedDetails includes: auditDetail)
								ifFalse: [ 
									"package audit details do not have associated behavior, so skip this clause"
									auditDetail isPackageDetail
										ifFalse: [ 
											| behavior |
											behavior := auditDetail behavior.
											auditDetail selector
												ifNotNil: [ :theSelector | 
													(missingLoadedNotIdenticalCandidateMap at: behavior ifAbsent: [  ])
														ifNotNil: [ :selectorDetailMap | 
															(selectorDetailMap at: theSelector ifAbsent: [  ])
																ifNotNil: [ :detailDict | 
																	self
																		repairMissingLoadedNotIdenticalFor: theSelector
																		detailDict: detailDict
																		repairedDetails: repairedDetails
																		forPackageName: packageName
																		andBehavior: behavior ] ] ] ].
									(repairedDetails includes: auditDetail)
										ifFalse: [ 
											| message repairSelector |
											reason := auditDetail reason.
											message := auditDetail message.
											self errorMessages add: message.
											repairSelector := repairMap
												at: reason
												ifAbsent: [ 
													self skip
														ifTrue: [ #'skipRepair:' ]
														ifFalse: [ self error: 'unrepairable audit error: ' , message printString ] ].
											self perform: repairSelector with: auditDetail ] ] ] ] ].
	self repairSummary
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairClassesNotIdentical: className inPackageNamed: packageName [
	| loadedClass loadedPackage theClass |
	loadedPackage := (self globalNamed: 'Rowan') image
		loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'  Could not repair classes not identical: ' , className , ' for package '
						, packageName , ' (No loaded class or loaded extension class found for '
						, className printString , ' in package ' , packageName , ')' ].
	theClass := Rowan globalNamed: className.
	loadedClass handle ~~ theClass
		ifFalse: [ 
			"confirm the audit issue -- classes not identical"
			self
				error:
					'Invalid method audit details for repair of classesNotIdentical. The class named '
						, className printString
						, ' does match the handle for the loaded class in package '
						, packageName printString ].
	(loadedPackage loadedClasses includesKey: className)
		ifTrue: [ loadedPackage loadedClasses removeKey: className ]
		ifFalse: [ loadedPackage loadedClassExtensions removeKey: className ].	"remove the orphaned loaded class"
	Rowan packageTools adopt adoptClass: theClass intoPackageNamed: packageName.	"adopt the class into packageName ...when gemstonBaseImage loaded, the loaded class is expected to be moved to the correct package"
	self
		logMessage:
			'  Repair classes not identical: ' , loadedClass name , ' for package '
				, packageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairDifferentMethodCategory: auditDetail [
	"different method categories are repaired by moving the method to the expected category"

	self
		repairDifferentMethodCategory:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		expectedCategory: auditDetail category
		inBehavior: auditDetail behavior
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairDifferentMethodCategory: methodSpec expectedCategory: category inBehavior: aBehavior inPackageNamed: packageName [
	"missing compiled methods (because upgradeImage has removed them) can be repaired by 
		removing the associated loaded method, which will allow the method to be loaded 
		into the image, when project is reloaded"

	| selector |
	selector := self selectorFromMethodSpec: methodSpec.
	(aBehavior includesCategory: category)
		ifFalse: [ aBehavior addCategory: category ].
	aBehavior moveMethod: selector toCategory: category environmentId: 0.
	self
		logMessage:
			'  Repair different method category: ' , aBehavior printString , '>>' , selector
				, ' to ' , category , ' for package ' , packageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'accessing' }
UpgradeRowanV2 >> repairedByReload [
	^ repairedByReload ifNil: [ repairedByReload := 0 ].

]

{ #category : 'accessing' }
UpgradeRowanV2 >> repairedCount [
	^ repairedCount ifNil: [ repairedCount := 0 ].

]

{ #category : 'repair' }
UpgradeRowanV2 >> repairedWhenDefinitionsReloaded:ignoredMethod [ 
	repairedByReload := self repairedByReload + 1
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairLoadedPackageNotInRegistry: auditDetail [
	"loadedPackageNotInRegistry occur because a symbol dictionary where 
		the package is registered is no longer in the symbol list. To repair
		we put the symbol dictionary in the transient symbol list and disown
		the package."

	| loadedPackage symbolDictName sess transientSymbolList symbolDict |
	loadedPackage := auditDetail owner.
	symbolDictName := loadedPackage packageSymbolDictionaryName.
	(#('GsCompilerClasses' 'ObsoleteClasses') includes: symbolDictName)
		ifFalse: [ 
			self
				error:
					'The symbol dictionary ' , symbolDictName printString
						, ' for the loaded package ' , loadedPackage key printString
						,
							' is not in the list of repairable symbol dictionaries (GsCompilerClasses)' ].
	sess := GsCurrentSession currentSession.
	sess _transientSymbolList ifNil: [ System refreshTransientSymbolList ].
	transientSymbolList := sess transientSymbolList.
	symbolDict := Globals at: symbolDictName asSymbol.
	transientSymbolList insertObject: symbolDict at: 1.

	Rowan packageTools disown disownPackageNamed: loadedPackage key
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairMissingCompiledMethod: auditDetail [
	"missing compiled methods (because upgradeImage has removed them) can be repaired by 
		removing the associated loaded method, which will allow the method to be loaded 
		into the image, when project is reloaded"

	self
		repairMissingCompiledMethod:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairMissingCompiledMethod: methodSpec inClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	"missing compiled methods (because upgradeImage has removed them) can be repaired by 
		removing the associated loaded method, which will allow the method to be loaded 
		into the image, when project is reloaded"

	| loadedClass loadedPackage selector loadedMethod |
	loadedPackage := (self globalNamed: 'Rowan') image
		loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'  Could not repair missing compiled method: ' , loadedClass name
						,
							(isMeta
								ifTrue: [ ' class' ]
								ifFalse: [ '' ]) , '>>' , selector , ' for package ' , packageName
						, ' (No loaded class or loaded extension class found for '
						, className printString , ' in package ' , packageName , ')' ].
	selector := self selectorFromMethodSpec: methodSpec.
	loadedMethod := isMeta
		ifTrue: [ loadedClass loadedClassMethods at: selector ]
		ifFalse: [ loadedClass loadedInstanceMethods at: selector ].
	loadedClass removeLoadedMethod: loadedMethod.
	(loadedClass name = 'FileLocator'
		and: [ isMeta and: [ selector == #'rowanProjectsHome' ] ])
		ifTrue: [ 
			| theBehavior |
			"The method is sent while reloading Rowan, so we need to patch this puppy and rely on the reload step to properly implement it if changed"
			theBehavior := (self globalNamed: 'FileLocator') class.
			theBehavior
				rwCompileExtensionMethod:
					'rowanProjectsHome
	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ self origin: #rowanProjectsHome'
				category: '*', packageName asLowercase
				packageName: packageName ].
	self
		logMessage:
			'  Repair missing compiled method: ' , loadedClass name
				,
					(isMeta
						ifTrue: [ ' class' ]
						ifFalse: [ '' ]) , '>>' , selector , ' for package ' , packageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairMissingLoadedMethod: auditDetail [
	"missing loaded methods are a special case of missing compiled method, 
		where the compiled method was initially removed and then replaced 
		by a new compiled method thus losing the reference the loaded method, 
		can be repaired by splicing the loaded method into the new compiled method"

	self
		repairMissingLoadedMethod:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedClassOrClassExtension loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairMissingLoadedMethod: methodSpec inClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	"There are two use cases for missing loaded methods:
		1. the reference to the loaded method from the compiled method is missing,
			but the loaded method itself is present
		2. the reference to a loaded method from the compiled method is missing
			AND the loaded method itself is missing
		Missing loaded methods are a special case of missing compiled method, 
		where the compiled method was initially removed and then replaced 
		by a new compiled method thus losing the reference the loaded method, 
		can be repaired by splicing the loaded method into the new compiled method"

	| loadedClass loadedPackage selector loadedMethod theCompiledMethod theBehavior thePackageName |
	thePackageName := packageName.
	loadedPackage := (self globalNamed: 'Rowan') image
		loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'  Could not repair missing loaded method: ' , loadedClass name
						,
							(isMeta
								ifTrue: [ ' class' ]
								ifFalse: [ '' ]) , '>>' , selector
						, ' (No loaded class or loaded extension class found for '
						, className printString , ')' ].
	selector := self selectorFromMethodSpec: methodSpec.
	theBehavior := loadedClass handle.
	isMeta
		ifTrue: [ theBehavior := theBehavior class ].
	(theCompiledMethod := theBehavior
		compiledMethodAt: selector asSymbol
		environmentId: 0
		otherwise: nil)
		ifNil: [ 
			self
				error:
					'  Could not repair missing loaded method: ' , loadedClass name
						,
							(isMeta
								ifTrue: [ ' class' ]
								ifFalse: [ '' ]) , '>>' , selector , ' (No compiled method ' , className
						,
							(isMeta
								ifTrue: [ ' class>>' ]
								ifFalse: [ '>>' ]) , selector , ' found)' ].
	loadedMethod := isMeta
		ifTrue: [ loadedClass loadedClassMethods at: selector ifAbsent: [  ] ]
		ifFalse: [ loadedClass loadedInstanceMethods at: selector ifAbsent: [  ] ].
	loadedMethod
		ifNil: [ 
			"at the time of the audit failure, the actual package involved is not known for certain, 
				so if there is no loaded method for the selector in the loadedClass associated with
				given package, then we need to scan the loaded class extensions for this class to
				find the correct loaded method"
			(Rowan image loadedClassExtensionsForClass: theBehavior theNonMetaClass)
				do: [ :loadedClassExtension | 
					loadedMethod
						ifNil: [ 
							loadedMethod := isMeta
								ifTrue: [ loadedClassExtension loadedClassMethods at: selector ifAbsent: [  ] ]
								ifFalse: [ loadedClassExtension loadedInstanceMethods at: selector ifAbsent: [  ] ].
							thePackageName := loadedClassExtension packageName ] ].
			loadedMethod
				ifNil: [ 
					"If we are still unable to find a loaded method, then we assume that this is a 
						newly added kernel method for this version of GemStone ... adopt the 
						method into the loadedPackage (any package will do) and expect correction 
						during post repair load"
					Rowan packageTools adopt
						adoptMethod: selector
						inClassNamed: className
						isMeta: isMeta
						intoPackageNamed: packageName.
					self
						logMessage:
							'  Adopt missing loaded method: ' , loadedClass name
								,
									(isMeta
										ifTrue: [ ' class' ]
										ifFalse: [ '' ]) , '>>' , selector , ' in package named '
								, thePackageName.
					repairedCount := self repairedCount + 1.
					^ self ] ].
	theCompiledMethod _rowanPackageInfo: loadedMethod.
	loadedMethod handle: theCompiledMethod.
	self
		logMessage:
			'  Repair missing loaded method: ' , loadedClass name
				,
					(isMeta
						ifTrue: [ ' class' ]
						ifFalse: [ '' ]) , '>>' , selector , ' for package ' , thePackageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairMissingLoadedNotIdentical: candidateDetailMap [
	| repairedDetails |
	repairedDetails := IdentitySet new.
	^ repairedDetails
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairMissingLoadedNotIdenticalFor: theSelector detailDict: detailDict repairedDetails: repairedDetails forPackageName: packageName andBehavior: aBehavior [
	"
		When we find the pattern of a pair of auditDetails for the same package, class 
		and class, where one reason is #'missingLoadedMethodDetail' and the other 
		reason is #'methodsNotIdenticalDetail', they need to be repaired at the same 
		time using this method
	"

	| methodsNotIdenticalDetail loadedClass theMethod loadedClasses theClass |
	methodsNotIdenticalDetail := detailDict at: #'methodsNotIdenticalDetail'.
	theClass := aBehavior theNonMetaClass.
	loadedClasses := Rowan image loadedClassExtensionsForClass: theClass.
	(Rowan image loadedClassForClass: theClass ifAbsent: [  ])
		ifNotNil: [ :lc | loadedClasses add: lc ].
	loadedClass := loadedClasses
		detect: [ :loadedClassOrExtension | loadedClassOrExtension loadedPackage name = packageName ].	"it IS possible that there will be no matching package, but we'll have to deal with that later"
	theMethod := aBehavior compiledMethodAt: theSelector.
	theMethod == methodsNotIdenticalDetail method
		ifFalse: [ 
			self
				error:
					'Invalid method audit details for repair of methodsNotIdentical and missingLoadedMethod audit error' ].
	(aBehavior isMeta
		ifTrue: [ loadedClass loadedClassMethods at: theSelector ifAbsent: [  ] ]
		ifFalse: [ loadedClass loadedInstanceMethods at: theSelector ifAbsent: [  ] ])
		ifNotNil: [ :loadedMethod | 
			loadedMethod == methodsNotIdenticalDetail loadedMethod
				ifFalse: [ 
					self
						error:
							'Invalid loaded method audit details for repair of methodsNotIdentical and missingLoadedMethod audit error' ].
			loadedMethod handle: theMethod.
			theMethod _rowanPackageInfo: loadedMethod.
			repairedDetails
				add: methodsNotIdenticalDetail;
				add: (detailDict at: #'missingLoadedMethodDetail');
				yourself.
			self
				logMessage:
					'  Repair missing loaded method and nonidentical method: '
						, aBehavior printString , '>>' , theSelector , ' for package '
						, packageName ]
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairNonIdenticalMethod: auditDetail [
	"nonIdentical methods (because upgradeImage has removed and recompiled the 
		compiled method, leaving the loaded method referencing a dangling compiled 
		method reference) can be repaired by slicing the new compiled method into 
		the loaded method. The method to be loaded into the image, if the source of
		the new method doesn't match the source of the incoming method."

	self
		repairNonIdenticalMethodFor:
			auditDetail behavior printString , ' >> ' , auditDetail selector
		inClassNamed: auditDetail behavior theNonMetaClass name asString
		isMeta: auditDetail behavior isMeta
		inPackageNamed: auditDetail loadedMethod loadedPackage name
]

{ #category : 'repair' }
UpgradeRowanV2 >> repairNonIdenticalMethodFor: methodSpec inClassNamed: className isMeta: isMeta inPackageNamed: packageName [
	"nonIdentical methods (because upgradeImage has removed and recompiled the 
		compiled method, leaving the loaded method referencing a dangling compiled 
		method reference) can be repaired by slicing the new compiled method into 
		the loaded method. The method to be loaded into the image, if the source of
		the new method doesn't match the source of the incoming method."

	| loadedMethod loadedClass loadedPackage loadedProject selector theClass theBehavior oldCompiledMethod newCompiledMethod |
	loadedPackage := (self globalNamed: 'Rowan') image
		loadedPackageNamed: packageName.
	loadedClass := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'  Could not repair  nonidentical method: ' , theBehavior printString , '>>'
						, selector , ' for package ' , packageName
						, ' (No loaded class or loaded extension class found for '
						, className printString , ' in package ' , packageName , ')' ].
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
			self
				logMessage:
					'identical compiled methods when non-identical compiled methods expected for '
						, className
						,
							(isMeta
								ifTrue: [ ' class >> ' ]
								ifFalse: [ ' >> ' ]) , selector printString , ' for package '
						, packageName , ' no repair necessary'.
			^ self ].

	oldCompiledMethod _rowanPackageInfo: nil.
	loadedMethod handle: newCompiledMethod.
	newCompiledMethod _rowanPackageInfo: loadedMethod.
	self
		logMessage:
			'  Repair nonidentical method: ' , theBehavior printString , '>>' , selector
				, ' for package ' , packageName.
	repairedCount := self repairedCount + 1.
]

{ #category : 'upgrade' }
UpgradeRowanV2 >> repairSummary [
	self logMessage: 'REPAIR SUMMARY'.
	self
		logMessage: 'Repaired ' , self repairedCount printString , ' audit failures'.
	self
		logMessage:
			'Repaired ' , self repairedByReload printString , ' by RELOAD audit failures'.
	self logMessage: 'Skipped ' , self skipCount printString , ' audit repairs'
]

{ #category : 'upgrade rowan' }
UpgradeRowanV2 >> rowanProjectNames [
	^ #('Rowan' 'STON' 'RowanClientServices' 'Cypress')
]

{ #category : 'upgrade rowan' }
UpgradeRowanV2 >> rowanRepairMap [
	"List of audit errors that would be expected in Rowan code if methods were 
		removed from classes managed by Rowan without updating the Rowan 
		metadata, as happens during upgradeImage"

	| repairMap |
	repairMap := Dictionary new.
	repairMap
		at: #'methodsNotIdentical' put: #'repairNonIdenticalMethod:';
		at: #'differentMethodCategory' put: #'repairDifferentMethodCategory:';
		at: #'differentComment' put: #'repairedWhenDefinitionsReloaded:';
		at: #'missingCompiledMethod' put: #'repairMissingCompiledMethod:';
		at: #'missingLoadedMethod' put: #'repairMissingLoadedMethod:';
		at: #'missingCompiledMethodsForLoadedClassExtension'
			put: #'repairMissingCompiledMethod:';
		yourself.
	^ repairMap
]

{ #category : 'private' }
UpgradeRowanV2 >> selectorFromMethodSpec: methodSpec [
	| index |
	index := methodSpec indexOfSubCollection: ' >> ' startingAt: 1.
	^ (methodSpec copyFrom: index + 4 to: methodSpec size) asSymbol

]

{ #category : 'accessing' }
UpgradeRowanV2 >> shouldCommit [
	^ shouldCommit ifNil: [ shouldCommit := false ]
]

{ #category : 'accessing' }
UpgradeRowanV2 >> shouldCommit: object [
	shouldCommit := object
]

{ #category : 'accessing' }
UpgradeRowanV2 >> skip [
	^ skip ifNil: [ skip := true ]
]

{ #category : 'accessing' }
UpgradeRowanV2 >> skipCount [
	^ skipCount ifNil: [ skipCount := 0 ]

]

{ #category : 'repair' }
UpgradeRowanV2 >> skipRepair: auditDetail [
	skipCount := self skipCount + 1
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_1_repairRowanAuditFailures [
	"Use the Rowan project audit to repair the damaged Rowan metadata"

	self logMessage: ' repair ROWAN audit failures'.
	self auditForProjectsNamed: self rowanProjectNames.
	self logMessage: ' repairing ROWAN audit failures'.
	audit isEmpty
		ifFalse: [ 
			repairedCount := 0.
			self repairAuditFailures: self rowanRepairMap.
			self commit ].
	self
		logMessage:
			' ROWAN audit repair COMPLETE (' , repairedCount printString , ' repairs)'
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_2_repairGemstoneBaseAuditFailures [
	"Use the gemstoneBaseImage project audit to repair the damaged Rowan metadata"

	self logMessage: ' repair gemstoneBaseImage audit failures'.
	self auditForProjectsNamed: self gemstoneBaseProjectNames.
	self logMessage: ' repairing gemstoneBaseImage audit failures'.
	audit isEmpty
		ifFalse: [ 
			repairedCount := 0.
			self repairAuditFailures: self gemstoneBaseRepairMap.
			self commit ].
	self
		logMessage:
			' gemstoneBaseImage audit repair COMPLETE (' , repairedCount printString
				, ' repairs)'
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_3_repairCustomerAuditFailures [
	"Use the Rowan project audit to repair any damaged customer metadata, caused by the removal of base extension methods"

	self logMessage: ' repair CUSTOMER audit failures'.
	self auditForProjectsNamed: self customerProjectNames.
	self logMessage: ' repairing CUSTOMER audit failures'.
	audit isEmpty
		ifFalse: [ 
			repairedCount := 0.
			self repairAuditFailures: self customerRepairMap.
			self commit ].
	self
		logMessage:
			' CUSTOMER audit repair COMPLETE (' , repairedCount printString , ' repairs)'
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_4_reloadRowan [
	"Reload the Rowan project to repair those audit failures repaired by a reload"

	| rowanVersion |
	rowanVersion := Rowan version.
	self logMessage: 'reload Rowan'.
	self logMessage: '	original Rowan version: ', rowanVersion printString.
	self reloadRowan.
	self commit
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_5_reloadGemstoneBase [
	"Reload the gemstoneBaseImage project to repair those audit failures repaired by a reload and to install platform-specific changes"

	self logMessage: 'reload gemstoneBaseImage project'.
	self reloadGemstoneBase.
	self commit
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_6_reloadCustomer [
	"Reload the Customer projects to repair those audit failures repaired by a reload and to install platform-specific changes"

	self logMessage: 'reload Customer projects'.
	self reloadCustomer.
	self commit
]

{ #category : 'steps' }
UpgradeRowanV2 >> step_7_finalAudit [
	self logMessage: 'Final Audit'.
	self auditForProjectsNamed: (self globalNamed: 'Rowan') projectNames.
	self countAuditErrors.
	audit isEmpty
		ifFalse: [ 
			self
				error:
					'Final Audit did not run clean ... repair audit errors before proceeding' ]
]

{ #category : 'accessing' }
UpgradeRowanV2 >> upgradeFrom [
	upgradeFrom ifNil: [ self error: 'upgradeFrom must be defined' ].
	^ upgradeFrom
]

{ #category : 'accessing' }
UpgradeRowanV2 >> upgradeFrom: object [
	upgradeFrom := object
]
