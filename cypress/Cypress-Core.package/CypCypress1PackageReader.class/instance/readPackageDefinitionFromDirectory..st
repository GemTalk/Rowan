"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
readPackageDefinitionFromDirectory: fullPath

	| packageDefinition |
	packageDefinition := CypPackageDefinition new.
	(disk directoryEntriesFrom: fullPath) do: 
			[:entry |
			(self isPropertiesFilePath: entry)
				ifTrue: 
					[packageDefinition properties: (self readPropertiesFileFromPath: entry)].
			(entry endsWith: '.class')
				ifTrue: 
					[packageDefinition
						addClassDefinition: (self readClassDefinitionFromPath: entry)].
			(entry endsWith: '.extension')
				ifTrue: 
					[packageDefinition
						addClassExtension: (self readClassExtensionDefinitionFromPath: entry)]].
	^packageDefinition