"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
readMethodDefinitionsFor: classDefinition in: entries

	entries do: 
			[:entry |
			| methods isMeta |
			(entry endsWith: '/instance') ifTrue: [isMeta := false].
			(entry endsWith: '/class') ifTrue: [isMeta := true].
			isMeta
				ifNotNil: 
					[((disk directoryEntriesFrom: entry) select: [:each | each endsWith: '.st'])
						do: 
							[:methodEntry |
							disk readStreamFor: methodEntry
								do: 
									[:fileStream |
									| methodDefinition |
									methodDefinition := self readMethodDefinitionFrom: fileStream.
									isMeta
										ifTrue: [classDefinition addClassMethodDefinition: methodDefinition]
										ifFalse: [classDefinition addInstanceMethodDefinition: methodDefinition]]]]]