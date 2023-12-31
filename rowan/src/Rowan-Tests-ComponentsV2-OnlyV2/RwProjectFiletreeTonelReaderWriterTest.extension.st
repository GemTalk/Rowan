Extension { #name : 'RwProjectFiletreeTonelReaderWriterTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProject [

	| rowanProject projectHome specUrlString projectDefinitionSet loadSpec |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
    projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/v2/rowan/specs/Issue122_', self _repositoryFormat, '.ston'.

"create load spec"
	loadSpec :=  (RwLoadSpecificationV2 fromUrl: specUrlString)
		projectsHome: projectHome;
		diskUrl:  'file:' , projectHome, '/Issue122/v2';
		yourself.

"read project"
	projectDefinitionSet := RwProjectSetDefinition new
		addProject: loadSpec resolveProject;
		yourself.

"validation"
	self _validateIssue122ProjectDefinitionSet: projectDefinitionSet projectName: loadSpec projectName
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithBothMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet patches loadSpec |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/v2/rowan/specs/Issue122_Both-ProtocolValidationError_', self _repositoryFormat, '.ston'.

"create load spec"
	loadSpec :=  (RwLoadSpecificationV2 fromUrl: specUrlString)
		projectsHome: projectHome;
		diskUrl:  'file:' , projectHome, '/Issue122/v2';
		yourself.

"read project -- catch and resume the notification ... repair the protocol"
	[ 
		projectDefinition := loadSpec resolveProject.
		projectDefinitionSet := RwProjectSetDefinition new
			addProject: projectDefinition;
			yourself ]
		on: RwInvalidCategoryProtocolConventionErrorNotification , RwUnreadPackagesErrorNotification
		do: [:ex | 
			(ex isKindOf: RwUnreadPackagesErrorNotification)
				ifTrue: [
					| unreadPackageNames |
					"Handle and resume, allowing the read to continue without error ... allowing for repair"
					unreadPackageNames := ex unreadPackageNames.
					self assert: unreadPackageNames size = 1.
					self assert: (unreadPackageNames includes: 'Issue122-Core-ProtocolValidationErro').
					ex resume]
				ifFalse: [ 
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
							ex resume ] ] ].

"validation - read from disk, without repairing the issues (creating a patch reads without repairing validation errors"
	[patches := Rowan projectTools diff patchesForProjectDefinition: projectDefinition]
		on: RwUnreadPackagesErrorNotification
		do: [:ex | 
			"Handle and resume, allowing the read to continue without error ... allowing for repair"
			ex resume ].
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

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet loadSpec |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/v2/rowan/specs/Issue122_CategoryValidationError_', self _repositoryFormat, '.ston'.


"create load spec"
	loadSpec :=  (RwLoadSpecificationV2 fromUrl: specUrlString)
		projectsHome: projectHome;
		diskUrl:  'file:' , projectHome, '/Issue122/v2';
		yourself.

"read project -- hit category does not match error"
	self 
		should: [ projectDefinition := loadSpec resolveProject ]
		raise: Error.

"read project -- catch and resume the notification ... repair the category"
	[ 
		projectDefinition := loadSpec resolveProject.
		projectDefinitionSet := RwProjectSetDefinition new
			addProject: projectDefinition;
			yourself  ]
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


	| rowanProject projectHome projectName specUrlString  projectDefinitionSet  loadSpec|	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	projectName := 'Issue361'.
	specUrlString :=  'file:' , projectHome, '/', projectName, '/v2/rowan/specs/Issue122_', self _repositoryFormat, '.ston'.

"create load spec"
	loadSpec :=  (RwLoadSpecificationV2 fromUrl: specUrlString)
		projectsHome: projectHome;
		diskUrl:  'file:' , projectHome, '/', projectName, '/v2';
		yourself.

"read project -- hit protocol is invalid error"
	projectDefinitionSet := RwProjectSetDefinition new
		addProject: loadSpec resolveProject;
		yourself .

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

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet loadSpec |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString := 'file:' , projectHome, '/Issue122/v2/rowan/specs/Issue122_Extension-ProtocolValidationError_', self _repositoryFormat, '.ston'.

"create load spec"
	loadSpec :=  (RwLoadSpecificationV2 fromUrl: specUrlString)
		projectsHome: projectHome;
		diskUrl:  'file:' , projectHome, '/Issue122/v2';
		yourself.

"read project -- hit protocol is invalid error"
	self 
		should: [ 
			projectDefinition := loadSpec resolveProject.
			projectDefinitionSet := RwProjectSetDefinition new
				addProject: projectDefinition;
				yourself ]
		raise: Error.

"read project -- catch and resume the notification ... repair the protocol"
	[ 
		projectDefinition := loadSpec resolveProject.
		projectDefinitionSet := RwProjectSetDefinition new
			addProject: projectDefinition;
			yourself ]
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

	| rowanProject projectHome specUrlString projectDefinition projectDefinitionSet loadSpec |	
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath , '/test/testRepositories/'.
"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/v2/rowan/specs/Issue122_Core-ProtocolValidationError_', self _repositoryFormat, '.ston'.

"create load spec"
	loadSpec :=  (RwLoadSpecificationV2 fromUrl: specUrlString)
		projectsHome: projectHome;
		diskUrl:  'file:' , projectHome, '/Issue122/v2';
		yourself.

"read project -- hit protocol is invalid error"
	self 
		should: [ 
			projectDefinition := loadSpec resolveProject.
			projectDefinitionSet := RwProjectSetDefinition new
				addProject: projectDefinition;
				yourself ]
		raise: Error.

"read project -- catch and resume the notification ... repair the protocol"
	[ 
		projectDefinition := loadSpec resolveProject.
		projectDefinitionSet := RwProjectSetDefinition new
			addProject: projectDefinition;
			yourself ]
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
