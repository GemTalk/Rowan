"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
