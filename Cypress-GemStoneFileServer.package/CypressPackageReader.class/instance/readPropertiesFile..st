reading
readPropertiesFile: entry

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]