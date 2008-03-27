package lslplus.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;

public class LslStackFrame implements IStackFrame{
    private static final IRegisterGroup[] EMPTY_REGISTER_GROUP =
        new IRegisterGroup[0];

    private String name;
    private IThread thread;
    private IVariable[] variables;
    private boolean stepping;
    public LslStackFrame(
            String name, 
            IThread thread,
            IDebugTarget debugTarget,
            IVariable[] variables) {
        this.name = name;
        this.thread = thread;
        this.variables = variables;
    }
    
    public int getCharEnd() throws DebugException {
        // TODO Auto-generated method stub
        return 0;
    }

    public int getCharStart() throws DebugException {
        // TODO Auto-generated method stub
        return 0;
    }

    public int getLineNumber() throws DebugException {
        // TODO Auto-generated method stub
        return 0;
    }

    public String getName() throws DebugException {
        return name;
    }

    public IRegisterGroup[] getRegisterGroups() throws DebugException {
        return EMPTY_REGISTER_GROUP;
    }

    public IThread getThread() {
        return thread;
    }

    public IVariable[] getVariables() throws DebugException {
        return variables;
    }

    public boolean hasRegisterGroups() throws DebugException {
        return false;
    }

    public boolean hasVariables() throws DebugException {
        return variables != null && variables.length > 0;
    }

    public IDebugTarget getDebugTarget() {
        return thread.getDebugTarget();
    }

    public ILaunch getLaunch() {
        return thread.getLaunch();
    }

    public String getModelIdentifier() {
        return getDebugTarget().getModelIdentifier();
    }

    public Object getAdapter(Class adapter) {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean canStepInto() {
        return !isStepping();
    }

    public boolean canStepOver() {
        return !isStepping();
    }

    public boolean canStepReturn() {
        return !isStepping();
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

    public boolean canResume() {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean canSuspend() {
        // TODO Auto-generated method stub
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

}
