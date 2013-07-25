reading
readClassPropertiesDictFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/properties.json']
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]