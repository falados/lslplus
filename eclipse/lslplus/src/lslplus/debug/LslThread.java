package lslplus.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

public class LslThread implements IThread {

    private LslStackFrame[] stackFrames;
    private LslDebugTarget target;
    private boolean active;
    private boolean suspended;
    private boolean stepping;
    
    public LslThread(LslDebugTarget target) {
        this.target = target;
        this.active = true;
        this.suspended = false;
        this.stepping = false;
        stackFrames = new LslStackFrame[0];
    }

    public IBreakpoint[] getBreakpoints() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getName() throws DebugException {
        return "LslThread"; //$NON-NLS-1$
    }

    public int getPriority() throws DebugException {
        return 0;
    }

    public IStackFrame[] getStackFrames() throws DebugException {
        return stackFrames;
    }

    public IStackFrame getTopStackFrame() throws DebugException {
        if (stackFrames != null && stackFrames.length > 0) return stackFrames[0];
        return null;
    }

    public boolean hasStackFrames() throws DebugException {
        return (stackFrames != null && stackFrames.length > 0);
    }

    public void setStackFrames(LslStackFrame[] stackFrames) {
        if (stackFrames == null) stackFrames = new LslStackFrame[0];
        this.stackFrames = stackFrames;
    }
    
    public IDebugTarget getDebugTarget() {
        return target;
    }

    public ILaunch getLaunch() {
        return getDebugTarget().getLaunch();
    }

    public String getModelIdentifier() {
        return getDebugTarget().getModelIdentifier();
    }

    public Object getAdapter(Class adapter) {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean canResume() {
        // TODO Auto-generated method stub
        return active && suspended;
    }

    public boolean canSuspend() {
        // TODO Auto-generated method stub
        return active && !suspended;
    }

    public boolean isSuspended() {
        // TODO Auto-generated method stub
        return suspended;
    }

    public void resume() throws DebugException {
        // TODO Auto-generated method stub

    }

    public void suspend() throws DebugException {
        // TODO Auto-generated method stub

    }

    public boolean canStepInto() {
        return canResume();
    }

    public boolean canStepOver() {
        return canResume();
    }

    public boolean canStepReturn() {
        return canResume();
    }

    public boolean isStepping() {
        return stepping;
    }

    public void stepInto() throws DebugException {
        // TODO Auto-generated method stub

    }

    public void stepOver() throws DebugException {
        // TODO Auto-generated method stub

    }

    public void stepReturn() throws DebugException {
        // TODO Auto-generated method stub

    }

    public boolean canTerminate() {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean isTerminated() {
        // TODO Auto-generated method stub
        return false;
    }

    public void terminate() throws DebugException {
        // TODO Auto-generated method stub

    }

    public void setSuspended(boolean b) {
        this.suspended = b;
    }

}
