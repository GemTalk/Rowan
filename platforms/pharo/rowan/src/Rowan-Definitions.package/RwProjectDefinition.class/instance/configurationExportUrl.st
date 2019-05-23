properties
configurationExportUrl

	"Return an RwFileUrl suitable for saving the receiver's configuration in the appropriate configs directory"

	| exportPath |
	self repositoryUrl
		ifNil: [ self error: 'RepositoryUrl has not been set. Receiver has not bee associated with an existing Rowan project on disk.' ].
	exportPath := self repositoryRootPath , '/' , self configsPath , '/'.
	^('file:' , exportPath) asRwUrl
