"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutExtensionImplementationsOn: aStream
  | sortedExtensions |
  sortedExtensions := self extensions
    asSortedCollection: [ :a :b | a className <= b className ].
  self
    fileOut: 'Class Extension'
    implementationsFrom: sortedExtensions
    on: aStream
