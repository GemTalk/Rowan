"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream]