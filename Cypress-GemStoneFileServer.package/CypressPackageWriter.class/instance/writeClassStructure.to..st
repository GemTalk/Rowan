writing
writeClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: '.json'
			visit: [:fileStream | self writeClassStructure: classStructure on: fileStream]