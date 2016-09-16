"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: downloading
"
default
	"Answer a new URL with the receiver's path relative to the current working directory."
	
	self privateInitializeFromText: self pathString relativeTo: self class workingDirectory.
