reading
readClassPropertiesFromDirectoryEntries: entries
	"Look for Cypress .ston properties file first, but accept a .json version if present."

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/properties' , self propertiesFileNameExtension]
				ifNone: 
					[entries detect: [:entry | entry endsWith: '/properties.json']
						ifNone: [^Dictionary new]])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]