Extension { #name : 'RwProjectFiletreeTonelReaderWriterTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProject [

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
    projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

"create project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project"
	projectDefinitionSet := Rowan projectTools read 
		readProjectSetForComponentProjectDefinition: projectDefinition.

"validation"
	self _validateIssue122ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithBothMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet patches |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Both-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the protocol"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [ self assert: 'Unexpected notification ', ex class printString ]
				nonExtensionMethodNoficationDo: [ 
					"repair the protocol"
					ex methodDefinition protocol: 'repaired'.
					ex resume ] 
				extensionMethodPackageMismatchNoficationDo: [ 
					"repair the protocol - extension method protocol does start with a *, but does not match the package name"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ 
					"repair the protocol - extension method protocol doesn't start with a *"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ] ].

"validation - read from disk, without repairing the issues (creating a patch reads without repairing validation errors"
	patches := Rowan projectTools diff patchesForProjectDefinition: projectDefinition.
	self assert: patches size = 1.
	patches do: [:assoc |
		| packageName patch operations |
		packageName := assoc key.
		patch := assoc value.
		self assert: packageName = 'Issue122-Extension1-ProtocolValidationError'.
		operations := patch operations.
		self assert: operations size = 2.
		operations do: [:modification |
			| definition |
			definition := modification modification.
			definition selector = 'method1'
				ifTrue: [ self assert: definition category = '*issue122-extension1-protocolvalidationerror' ]
				ifFalse: [
					definition selector = 'method20'
						ifTrue: [ self assert: definition category = '*issue122-extension1-protocolvalidationerror' ]
						ifFalse: [ self assert: false description: 'unknown definition ', definition printString ] ] ] ]
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithClassCategoryValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"class category does not match package name -- Rowan Hybrid"

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_CategoryValidationError.ston'.

"create project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit category does not match error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.

"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the category"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [
					"repair the class category"
					ex classDefinition category: ex packageName.
					ex resume: true ]
				nonExtensionMethodNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] 
				extensionMethodPackageMismatchNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] ].

"validation"
	self _validateIssue122Repaired_CategoryValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithEmptyClassExtension [
	"https://github.com/GemTalk/Rowan/issues/361"


	| rowanProject projectHome projectName specUrlString projectDefinition projectDefinitionSet |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	projectName := 'Issue361'.
	specUrlString :=  'file:' , projectHome, '/', projectName, '/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

"create project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition.

"validate"
	self _validateIssue361ProjectDefinitionSet: projectDefinitionSet projectName: projectName
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithExtensionMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"
		extension method protocol doesn't start with a *
			OR
		extension method protocol does start with a *, but does not match the package name
	"

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Extension-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.

"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the protocol"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [ self assert: 'Unexpected notification ', ex class printString ]
				nonExtensionMethodNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] 
				extensionMethodPackageMismatchNoficationDo: [ 
					"repair the protocol - extension method protocol does start with a *, but does not match the package name"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ 
					"repair the protocol - extension method protocol doesn't start with a *"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ] ].

"validation"
	self _validateIssue122Repaired_ExtensionProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.
"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Core-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') newForUrl: specUrlString.

"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the protocol"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [ self assert: 'Unexpected notification ', ex class printString ]
				nonExtensionMethodNoficationDo: [ 
					"repair the protocol"
					ex methodDefinition protocol: 'repaired'.
					ex resume ] 
				extensionMethodPackageMismatchNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] ].

"validation"
	self _validateIssue122Repaired_CoreProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_Rowan_pharo [

	"write Rowan project to alternate directory, using the pharo project attributes to read the project in the first place"

	"execute without errors"

	| platformConfigurationAttributes projectDefinition projectSetDefinition projectSetModification visitor
		projectHome rowanProject projectAlias writerVisitorClass |

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := self _testRowanProjectsSandbox.
	projectAlias := 'Rowan_', self _repositoryFormat.

	(projectHome / projectAlias / 'src') 
		ensureCreateDirectory;
		deleteAllChildren;
		yourself.

"Read project and packages from disk for Pharo"
	platformConfigurationAttributes := {
		'common'.
		'pharo'.	"want to read pharo packages"
	}.
	projectDefinition := (Rowan globalNamed: 'RwComponentProjectDefinition') 
		newForUrl: 'file:', rowanProject repositoryRootPath, '/platforms/pharo/rowan/specs/Rowan_component.ston'.
	projectDefinition projectHome: rowanProject repositoryRootPath asFileReference parent.
	projectDefinition read: platformConfigurationAttributes.

"Write project and packages to disk in filetree format, for bootstrapping Pharo"
	projectDefinition
		packagesPath: 'src';
		projectHome: projectHome;
		projectAlias: projectAlias;
		packageFormat: self _repositoryFormat;
		yourself.
	projectSetDefinition := RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself.
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	writerVisitorClass := self _repositoryFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	visitor := writerVisitorClass new.
	visitor visit: projectSetModification.
]
