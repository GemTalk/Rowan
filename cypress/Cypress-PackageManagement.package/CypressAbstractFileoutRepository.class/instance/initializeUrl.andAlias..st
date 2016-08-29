"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeUrl: aUrl andAlias: aString

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
