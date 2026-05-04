package clojure.lang;

import uk.axvr.void_.V;

// NOTE: Ths doesn't actually work since Clojure's data structures are marked
// as "final", so cannot be inherited from.  This is likely a performance
// optimisation.

public class VoidablePersistentVector extends PersistentVector {

    static final class VoidableTransientVector extends VoidablePersistentVector.TransientVector {
        @Override public VoidableTransientVector assocN(int i, Object val) {
            if (V.VOID == val) { return this; } else { return super.assocN(i, val); }
        }
    }
}
