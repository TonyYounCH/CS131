import java.util.concurrent.atomic.AtomicLongArray;
class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public AtomicLongArray current() { return value; }

    public synchronized void swap(int i, int j) {
		value.addAndGet(i, -1);
		value.addAndGet(i, 1);
    }
}
