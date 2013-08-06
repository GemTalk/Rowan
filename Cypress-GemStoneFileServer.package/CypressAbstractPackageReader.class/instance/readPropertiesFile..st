"
notice: This work is protected by copyright. All rights reserved.
category: reading
"
readPropertiesFile: entry

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
