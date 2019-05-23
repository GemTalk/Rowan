tests
testReadExistingDiskProjectWithClassCategoryValidationError
	"https://github.com/GemTalk/Rowan/issues/122"

	"class category does not match package name -- Rowan Hybrid"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_CategoryValidationError.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit category does not match error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.

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