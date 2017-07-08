"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readExtensionClassStructureFromEntry: classEntry

	| classPropertiesDict entries classStructure |
	entries := self fileUtils directoryEntriesFrom: classEntry.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure
