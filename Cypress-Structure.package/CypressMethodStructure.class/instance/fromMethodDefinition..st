"
notice: This work is protected by copyright. All rights reserved.
category: initialization
"
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
