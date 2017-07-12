"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPackageStructure

	(self fileUtils directoryEntriesFrom: self packageDirectory) do: 
			[:entry |
			(self isPropertiesFileDirectoryEntry: entry)
				ifTrue: [self packageStructure properties: (self readPropertiesFile: entry)].
			(self string: entry endsWith: '.class')
				ifTrue: 
					[self packageStructure classes
						add: (self readClassStructureFromEntry: entry)].
			(self string: entry endsWith: '.extension')
				ifTrue: 
					[self packageStructure extensions
						add: (self readExtensionClassStructureFromEntry: entry)]]