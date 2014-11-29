# reusable-identifiers

Sometimes one wants to assign temporary unique and nice (short) identifiers and
then free them when no longer needed. This package makes that simple.

This implementation uses one bit per possible identifier. So, keeping a record
of 65535 (2^16) identifiers takes only 8kB, but (2^32) would take a whopping
512mB.
