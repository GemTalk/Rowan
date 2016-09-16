"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
