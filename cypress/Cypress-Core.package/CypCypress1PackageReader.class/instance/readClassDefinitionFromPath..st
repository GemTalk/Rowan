"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
readClassDefinitionFromPath: classDirectory

	| classPropertiesDict classComment entries classDefinition |
	entries := disk directoryEntriesFrom: classDirectory.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classComment := self readClassCommentFromDirectoryEntries: entries.
	classDefinition := self classDefinitionWithProperties: classPropertiesDict
				comment: classComment.
	self readMethodDefinitionsFor: classDefinition in: entries.
	^classDefinition