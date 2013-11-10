"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream]