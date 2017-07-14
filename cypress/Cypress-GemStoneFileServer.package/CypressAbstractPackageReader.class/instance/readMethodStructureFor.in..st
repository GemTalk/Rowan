"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readMethodStructureFor: classStructure in: entries

	entries do: 
			[:entry |
			| methods isMeta |
			methods := (isMeta := self string: entry endsWith: '/class')
						ifTrue: [classStructure classMethods]
						ifFalse: [classStructure instanceMethods].
			((self string: entry endsWith: '/instance')
				or: [self string: entry endsWith: '/class'])
					ifTrue: 
						[((self fileUtils directoryEntriesFrom: entry)
							select: [:each | self string: each endsWith: '.st']) do: 
									[:methodEntry |
									self fileUtils readStreamFor: methodEntry
										do: 
											[:fileStream |
											self
												readMethodStructureFrom: fileStream
												intoClassStructure: classStructure
												meta: isMeta
												methods: methods]]]]
