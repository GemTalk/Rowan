exporting
export

	| url exportUrl exportPath |
	self repositoryUrl
		ifNil: [ self error: 'exportToUrl: must be used if repositoryUrl has not been set' ].
	url := RwUrl fromString: self repositoryUrl.
	exportPath := self repositoryRootPath asFileReference / self specsPath.
	exportPath ensureCreateDirectory.
	exportUrl := exportPath fileSystem isMemoryFileSystem
		ifTrue: [ 'memory:' , exportPath pathString ]
		ifFalse: [ 'file:' , exportPath pathString, '/' ].
	^ self exportToUrl: exportUrl