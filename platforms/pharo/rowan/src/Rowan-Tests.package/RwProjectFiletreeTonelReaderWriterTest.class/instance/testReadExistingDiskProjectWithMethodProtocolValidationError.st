tests
testReadExistingDiskProjectWithMethodProtocolValidationError
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Core-ProtocolValidationError.ston'.

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
				nonExtensionMethodNoficationDo: [ 
					"repair the protocol"
					ex methodDefinition protocol: 'repaired'.
					ex resume ] 
				extensionMethodPackageMismatchNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] ].

"validation"
	self _validateIssue122Repaired_CoreProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name