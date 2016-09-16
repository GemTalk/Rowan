"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readClassPropertiesFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | self isPropertiesFileDirectoryEntry: entry]
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
