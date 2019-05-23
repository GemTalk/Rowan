tests
testReadExistingDiskProjectWithBothMethodProtocolValidationError
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet patches |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Both-ProtocolValidationError.ston'.

"create project definition"
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