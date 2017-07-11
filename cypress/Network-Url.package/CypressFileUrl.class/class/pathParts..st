"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
pathParts: aCollectionOfPathParts
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: true
