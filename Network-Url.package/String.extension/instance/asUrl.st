"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *network-url
"
asUrl
	"convert to a Url"
	"'http://www.cc.gatech.edu/' asUrl"
	"msw://chaos.resnet.gatech.edu:9000/' asUrl"
	^Url absoluteFromText: self