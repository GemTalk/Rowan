"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Network-Url
"
asUrl
	"Convert my path into a file:// type url - a FileUrl."
	
	^ FileUrl pathParts: (self directory pathSegments copyWith: self localName)