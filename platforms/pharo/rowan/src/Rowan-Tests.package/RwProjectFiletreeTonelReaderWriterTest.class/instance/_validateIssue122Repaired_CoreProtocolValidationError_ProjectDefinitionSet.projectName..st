private
_validateIssue122Repaired_CoreProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectName

	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 1.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core-ProtocolValidationError'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 1.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1.
								classDefinition instanceMethodDefinitions keysAndValuesDo: [:selector :methodDefinition | 
									self assert: selector = #method6.
									self assert: methodDefinition protocol = 'repaired' ] ]
							ifFalse: [  self assert: false description: 'unexpected class definition ', classDefinition name printString  ] ] ]
				ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ]