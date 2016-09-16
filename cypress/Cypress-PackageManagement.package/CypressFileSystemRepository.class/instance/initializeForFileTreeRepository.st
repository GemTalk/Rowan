"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeForFileTreeRepository

	self initializeDefaultRepositoryProperties.
	self
		alias: 'FileTree read-only repository on ', self directoryPath;
		codeFormatProperty: 'FileTree';
		strictCodeFormat: true.
