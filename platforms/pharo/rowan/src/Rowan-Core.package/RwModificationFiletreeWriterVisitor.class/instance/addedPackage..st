actions
addedPackage: aPackageModification

	| packageProperties |
	currentPackageDefinition := aPackageModification after.

	"create package directory and properties.json file"
	self _packageSourceDir ensureCreateDirectory.
	(self _packageSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
		aStream 
			<< '{'
			<< self  _newLine
			<< '	 }'].

	packageProperties := self _filetreePackagePropertyDictFor: aPackageModification.
	(packageProperties at:  'noMethodMetaData' ifAbsent: []) = self noMethodMetaData
		ifFalse: [
			"Rowan can read filetree repositories with methodMetaData, but does write method metadata"
			self error: 'method metadata not supported.' ].
	(packageProperties at:  'separateMethodMetaAndSource' ifAbsent: []) = self separateMethodMetaAndSource
		ifFalse: [
			"Rowan can read filetree repositories with separateMethodMetaAndSource, but does write separateMethodMetaAndSource"
			self error: 'separateMethodMetaAndSource not supported.' ].
	(packageProperties at:  'useCypressPropertiesFile' ifAbsent: []) = self useCypressPropertiesFile
		ifFalse: [
			"Rowan can read filetree repositories without CypressPropertiesFile, but does write without useCypressPropertiesFile"
			self error: 'only useCypressPropertiesFile not supported.' ].

	self processPackage: aPackageModification