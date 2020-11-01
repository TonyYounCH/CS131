import java.util.concurrent.atomic.AtomicLongArray;
class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() { 

		long[] long_val = new long[value.length()];
		for(int i = 0; i < value.length(); i++) {
			long_val[i] = (long)value.get(i);
		}
		return long_val;
    }

    public synchronized void swap(int i, int j) {
		value.getAndAdd(i, -1);
		value.getAndAdd(j, 1);
    }
}
