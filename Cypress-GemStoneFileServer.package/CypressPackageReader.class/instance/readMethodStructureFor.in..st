reading
readMethodStructureFor: classStructure in: entries

    entries
        do: [ :entry | 
            | methods isMeta |
 		methods := (isMeta := entry endsWith: '/class')
                ifTrue: [ classStructure classMethods ]
		    ifFalse: [ classStructure instanceMethods ].
            ((entry endsWith: '/instance') or: [entry endsWith: '/class' ])
                ifTrue: [ 
                    ((self fileUtils directoryEntriesFrom: entry) select: [ :each | each endsWith: '.st' ])
                        do: [ :methodEntry | 
                            self fileUtils readStreamFor: methodEntry
                                do: [ :fileStream | 
                                    | category source selector |
                                    category := fileStream nextLine trimSeparators.
                                    source := fileStream upToEnd.
						selector := UndefinedObject parseSelectorFrom: source.
                                     methods 
							at: selector
							put: ((CypressMethodStructure new)
									packageStructure: self packageStructure;
									classStructure: classStructure;
									name: selector;
									isMetaclass: isMeta;
									selector: selector;
									category: category;
									source: source;
									yourself) ] ] ] ]
