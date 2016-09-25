"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
updateCurrentOperations
  self
    currentOperations:
      ((snapshotDifferences at: 'operations')
        at: currentPackageName
        ifAbsentPut: [ OrderedCollection new ])