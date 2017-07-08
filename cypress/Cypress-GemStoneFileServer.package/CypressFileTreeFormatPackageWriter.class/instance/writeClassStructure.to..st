"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writeClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: self propertiesFileNameExtension
			visit: 
				[:fileStream |
				(self adjustClassPropertiesForFileTree: classStructure properties)
					writeCypressJsonOn: fileStream]
