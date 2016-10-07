"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Querying
"
allResolvedPackageReferences
  | resolved |
  resolved := OrderedCollection new.
  self knownRepositories
    keysAndValuesDo: [ :repoUrl :repo | 
      repo packageNames
        do: [ :packageName | resolved add: (CypressResolvedReference name: packageName repository: repo) ] ].
  ^ resolved asSortedCollection asArray