"
notice: This work is protected by copyright. All rights reserved.
category: reading
"
readClassPropertiesFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/properties', self propertiesFileNameExtension]
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
