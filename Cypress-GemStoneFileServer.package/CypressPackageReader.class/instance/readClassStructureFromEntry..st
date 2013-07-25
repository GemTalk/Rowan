reading
readClassStructureFromEntry: classEntry

	| classDirectory classPropertiesDict classComment entries classStructure |
	classDirectory := classEntry.
	entries := (self fileUtils directoryEntriesFrom: classDirectory).
	classPropertiesDict := self
				readClassPropertiesDictFromDirectoryEntries: entries.
	classComment := self readClassCommentFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict
				comment: classComment.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure