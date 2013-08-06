"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writeExtensionClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				writeCypressJsonOn: fileStream]