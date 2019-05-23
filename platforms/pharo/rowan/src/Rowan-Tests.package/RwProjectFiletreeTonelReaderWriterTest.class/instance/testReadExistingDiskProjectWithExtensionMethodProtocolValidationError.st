tests
testReadExistingDiskProjectWithExtensionMethodProtocolValidationError
	"https://github.com/GemTalk/Rowan/issues/122"

	"
		extension method protocol doesn't start with a *
			OR
		extension method protocol does start with a *, but does not match the package name
	"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Extension-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.

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