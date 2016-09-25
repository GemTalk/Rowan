"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initialize
  directoryPackageMap := Dictionary new.
  diskTimestamps := Dictionary new.
  diskSnapshots := Dictionary new.
  imageSnapshots := Dictionary new.
  snapshotDifferences := Dictionary new
    at: 'operations' put: Dictionary new;
    yourself