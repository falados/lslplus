package lslplus.debug;

import lslplus.LslPlusPlugin;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;

public class LslDebugTarget implements IDebugTarget {
    public static final String LSLPLUS = "lslplus"; //$NON-NLS-1$
    private String name;
    private IProcess process;
    private LslThread thread;
    private IThread[] threads;
    private ILaunch launch;
    public LslDebugTarget(String name, ILaunch launch, IProcess process) {
        this.name = name;
        this.process = process;
        this.launch = launch;
        thread = new LslThread(this);
        threads = new LslThread[] { thread };
    }

    public String getName() throws DebugException {
        return name;
    }

    public IProcess getProcess() {
        return process;
    }

    public IThread[] getThreads() throws DebugException {
        return threads;
    }

    public boolean hasThreads() throws DebugException {
        return true;
    }

    public boolean supportsBreakpoint(IBreakpoint breakpoint) {
        return (breakpoint instanceof ILineBreakpoint);
    }

    public String getModelIdentifier() {
        return LSLPLUS;
    }

    public boolean canTerminate() {
        // TODO Auto-generated method stub
        return !isTerminated();
    }

    public boolean isTerminated() {
        // TODO Auto-generated method stub
        return false;
    }

    public void terminate() throws DebugException {
        // TODO Auto-generated method stub

    }

    public boolean canResume() {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean canSuspend() {
        return false;
    }

    public boolean isSuspended() {
        // TODO Auto-generated method stub
        return false;
    }

    public void resume() throws DebugException {
        // TODO Auto-generated method stub

    }

    public void suspend() throws DebugException {
        // TODO Auto-generated method stub

    }

    public void breakpointAdded(IBreakpoint breakpoint) {
        // TODO Auto-generated method stub

    }

    public void breakpointChanged(IBreakpoint breakpoint, IMarkerDelta delta) {
        // TODO Auto-generated method stub

    }

    public void breakpointRemoved(IBreakpoint breakpoint, IMarkerDelta delta) {
        // TODO Auto-generated method stub

    }

    public boolean canDisconnect() {
        return false;
    }

    public void disconnect() throws DebugException {
        throw notSupported();
    }

    private DebugException notSupported() throws DebugException {
        return new DebugException(
                new Status(IStatus.ERROR,LslPlusPlugin.PLUGIN_ID,
                        DebugException.NOT_SUPPORTED,"",null));
    }

    public boolean isDisconnected() {
        return false;
    }

    public IMemoryBlock getMemoryBlock(long startAddress, long length) throws DebugException {
        throw notSupported();
    }

    public boolean supportsStorageRetrieval() {
        return false;
    }

    public IDebugTarget getDebugTarget() {
        return this;
    }

    public ILaunch getLaunch() {
        return launch;
    }

    public Object getAdapter(Class adapter) {

        if (ILaunch.class.equals(adapter)) {
            return getLaunch();
        }
        return null;
    }

}
