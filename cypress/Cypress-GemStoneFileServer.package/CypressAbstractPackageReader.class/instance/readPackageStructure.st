"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPackageStructure

   (self fileUtils directoryEntriesFrom: self packageDirectory)
        do: [ :entry | 
		(self isPropertiesFileDirectoryEntry: entry)
			ifTrue: [ self packageStructure properties: (self readPropertiesFile: entry) ].
            (entry endsWith: '.class')
                ifTrue: [ self packageStructure classes add: (self readClassStructureFromEntry: entry) ].
            (entry endsWith: '.extension')
                ifTrue: [ self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ] ]
