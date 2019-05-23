private
_validateIssue361ProjectDefinitionSet: projectDefinitionSet projectName: projectName

	"on disk the class extions directory/file exists, but is empty ... need to properly read from disk without errors"

	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 2.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 3.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1 ]
							ifFalse: [
								classDefinition name = 'Issue122Class2'
								ifTrue: [
									self assert: classDefinition category = packageName.
									self assert: classDefinition instanceMethodDefinitions size = 1.
									self assert: classDefinition classMethodDefinitions size = 0 ]
								ifFalse: [
									classDefinition name = 'Issue122Class3'
									ifTrue: [
										self assert: classDefinition instanceMethodDefinitions size = 1.
										self assert: classDefinition category = packageName.
										self assert: classDefinition classMethodDefinitions size = 1 ]
									ifFalse: [ self assert: false description: 'unexpected class definition ', classDefinition name printString ] ] ] ] ]
				ifFalse: [
					packageDefinition name = 'Issue122-Extension1'
						ifTrue: [
							self assert: packageDefinition classDefinitions isEmpty.
							self assert: packageDefinition classExtensions size = 1.
							packageDefinition classExtensions keysAndValuesDo: [:className :classExtension | 
								classExtension name = 'Issue122Class1'
									ifTrue: [ 
										self assert: classExtension instanceMethodDefinitions isEmpty.
										self assert: classExtension classMethodDefinitions isEmpty ]
									ifFalse: [ self assert: false description: 'unexpected classExtenstion definition ', classExtension name printString ] ] ]
						ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ] ]