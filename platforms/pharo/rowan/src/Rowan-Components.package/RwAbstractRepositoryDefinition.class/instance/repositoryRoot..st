accessing
repositoryRoot: pathStringOrReference

	| fileRef |
	fileRef := pathStringOrReference asFileReference.
	self projectUrl: nil.
	self gitRoot: nil.
	self properties at: 'repositoryRoot' put: fileRef