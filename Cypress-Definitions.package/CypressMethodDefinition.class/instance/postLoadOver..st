"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
