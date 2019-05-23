exporting
export

	|  exportUrl exportPath |
	exportPath := self _repositoryRootPath asFileReference / self _configsPath.
	exportPath ensureCreateDirectory.
	exportUrl := exportPath fileSystem isMemoryFileSystem
		ifTrue: [ 'memory:' , exportPath pathString ]
		ifFalse: [ 'file:' , exportPath pathString, '/' ].
	^ self exportToUrl: exportUrl