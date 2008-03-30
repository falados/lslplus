package lslplus.debug;

public interface Interactor {
    public void start();
    public void continueExecution();
    public void addListener(InteractorListener listener);
    public void removeListener(InteractorListener listener);
}
