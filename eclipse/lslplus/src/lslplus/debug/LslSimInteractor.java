package lslplus.debug;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.SimManager;
import lslplus.sim.SimEvent;
import lslplus.sim.SimEventListener;
import lslplus.sim.SimStatuses;
import lslplus.sim.SimStatuses.SimEnded;
import lslplus.sim.SimStatuses.SimInfo;
import lslplus.sim.SimStatuses.SimStatus;
import lslplus.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IBreakpoint;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * Interact with a running test session.
 */
public class LslSimInteractor implements Runnable, Interactor, SimEventListener {
    private static class BreakpointData {
        private static XStream xstream = new XStream(new DomDriver());
        public String file;
        public int line;
        public BreakpointData(String file, int line) {
            this.file = file;
            this.line = line;
        }
        
        public static void configureXStream(XStream xstream) {
            xstream.alias("breakpoint", BreakpointData.class); //$NON-NLS-1$
        }
        
        static {
            configureXStream(xstream);
        }
    }

    private abstract static class SimCommand {
        protected BreakpointData[] breakpoints = null;
        protected SimEvent[] events;
        protected SimCommand(BreakpointData[] breakpoints, SimEvent[] events) {
            this.breakpoints = breakpoints;
            this.events = events;
        }
    }
    
    private static class ContinueCommand extends SimCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public ContinueCommand(BreakpointData[] breakpoints, SimEvent[] events) {
            super(breakpoints, events);
        }
        
        static {
            xstream.alias("sim-continue", ContinueCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
            SimEvent.configureXStream(xstream);
        }
        
        public static String toXML(ContinueCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    private static class StepCommand extends SimCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public StepCommand(BreakpointData[] breakpoints, SimEvent[] events) {
            super(breakpoints, events);
        }
        
        static {
            xstream.alias("sim-step", StepCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(StepCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    private static class StepOverCommand extends SimCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public StepOverCommand(BreakpointData[] breakpoints, SimEvent[] events) {
            super(breakpoints, events);
        }
        
        static {
            xstream.alias("sim-step-over", StepOverCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(StepOverCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    private static class StepOutCommand extends SimCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public StepOutCommand(BreakpointData[] breakpoints, SimEvent[] events) {
            super(breakpoints, events);
        }
        
        static {
            xstream.alias("exec-step-out", StepOutCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(StepOutCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    
    private HashSet listeners = new HashSet();
    private BufferedReader reader;
    private PrintStream writer;
    private String simDescriptor;
    private Thread thread;
    private boolean done = false;
    private boolean debugMode;
    private LinkedList eventQueue = new LinkedList();
    
    public LslSimInteractor(String launchMode, String simDescriptor, InputStream in, OutputStream out) {
        reader = new BufferedReader(new InputStreamReader(in));
        writer = new PrintStream(out);
        
        this.simDescriptor = simDescriptor;
        this.debugMode = ILaunchManager.DEBUG_MODE.equals(launchMode);
    }
    
    public void start() {
        if (done || thread != null && thread.isAlive()) return;
        simManager().addSimEventListener(this);
        writeOut(simDescriptor);
        writeOut(continueText());
        thread = new Thread(this);
        thread.start();
    }
 
    public void stop() {
        simManager().removeSimEventListener(this);
    }
    
    private String continueText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        ContinueCommand cmd = new ContinueCommand(bpData,getAllPendingEvents());
        return ContinueCommand.toXML(cmd);
    }
    
    private String stepText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        StepCommand cmd = new StepCommand(bpData, getAllPendingEvents());
        return StepCommand.toXML(cmd);
    }

    private String stepOverText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        SimEvent[] events = (SimEvent[]) eventQueue.toArray(new SimEvent[0]);
        StepOverCommand cmd = new StepOverCommand(bpData,getAllPendingEvents());
        return StepOverCommand.toXML(cmd);
    }

    private String stepOutText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        StepOutCommand cmd = new StepOutCommand(bpData, getAllPendingEvents());
        return StepOutCommand.toXML(cmd);
    }

    private BreakpointData[] createBreakpointData() {
        IBreakpointManager bpm = getBreakpointManager();
        IBreakpoint[] breakpoints = bpm.getBreakpoints(LslDebugTarget.LSLPLUS);
        LinkedList list = new LinkedList();
        for (int i = 0; i < breakpoints.length; i++) {
            try {
                if (breakpoints[i] instanceof LslLineBreakpoint) {
                    LslLineBreakpoint bp = (LslLineBreakpoint) breakpoints[i];
                    int line = bp.getLineNumber();
                    IMarker marker = bp.getMarker();
                    IResource resource = marker.getResource();
                    IFile file = (IFile) resource.getAdapter(IFile.class);
                    if (file == null)
                        continue;
                    if (!marker.getAttribute(IBreakpoint.ENABLED,false)) continue;
                    IPath fullPath = file.getLocation();
                    list.add(new BreakpointData(fullPath.toOSString(), line));
                }
            } catch (CoreException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
        return (BreakpointData[]) list.toArray(new BreakpointData[list.size()]);
    }
    
    public void continueExecution() {
        sendCommand(continueText());
    }

    private void sendCommand(String commandText) {
        if (done || thread != null && thread.isAlive()) return;
        writeOut(commandText);
        thread = new Thread(this);
        thread.start();
    }
    
    public void step() {
        sendCommand(stepText());
    }
    
    public void stepOver() {
        sendCommand(stepOverText());
    }
    
    public void stepOut() {
        sendCommand(stepOutText());
    }
    
    public void addListener(InteractorListener listener) { listeners.add(listener); }
    public void removeListener(InteractorListener listener) { listeners.remove(listener); }
    
    public void close() {
        writer.close();
    }
    
    private void fireSuspended(LslScriptExecutionState state) {
        for (Iterator i = listeners.iterator(); i.hasNext();) {
            ((InteractorListener)i.next()).suspended(state);
        }
    }
    
    private void fireComplete() {
        for (Iterator i = listeners.iterator(); i.hasNext();) {
            ((InteractorListener)i.next()).completed();
        }
    }
    public void run() {
        String line = null;
        
        try {
            while ((line = reader.readLine()) != null) {
                //if (LslPlusPlugin.DEBUG) Util.log("read:" + Util.URIDecode(line)); //$NON-NLS-1$
                SimStatus status = SimStatuses.fromXML(Util.URIDecode(line));
                
                // kludge for the mo'
                if (status instanceof SimInfo) {
                    String cmd = continueText();
                    SimInfo info = (SimInfo)status;
                    simManager().newLogMessages(info.getMessages());
                    simManager().setSimState(info.getState());
                    //if (LslPlusPlugin.DEBUG) Util.log("writing: " + cmd); //$NON-NLS-1$
                    writeOut(cmd);
                } else if (status instanceof SimEnded) {
                    if (LslPlusPlugin.DEBUG) Util.log(Util.URIDecode(line));
                    SimEnded ended = (SimEnded) status;
                    simManager().newLogMessages(ended.getMessages());
                    simManager().setSimState(ended.getState());
                    endSession();
                    fireComplete();
                } else {
                    Util.error("Unrecognized status: " + status);
                }
            }
        } catch (IOException e) {
            Util.log(e, e.getLocalizedMessage());
        } catch (RuntimeException e) {
            Util.log(e, e.getLocalizedMessage());
            if (line != null) {
                Util.log("input was: " + Util.URIDecode(line));
            }
            try {
                endSession();
            } catch (Exception e1) {
            }
        }
    }

    private SimManager simManager() {
        return LslPlusPlugin.getDefault().getSimManager();
    }
    
    private DebugPlugin getDebugPlugin() { return DebugPlugin.getDefault(); }
    private IBreakpointManager getBreakpointManager() { return getDebugPlugin().getBreakpointManager(); }

    private void writeOut(String cmd) {
        writer.println(Util.URIEncode(cmd));
        writer.flush();
    }
    
    private void endSession() {
        writer.println("quit"); //$NON-NLS-1$
        writer.flush();
        writer.close();
    }
    
    public void putEvent(SimEvent event) {
        synchronized (eventQueue) {
            eventQueue.add(event);
        }
    }
    
    private SimEvent[] getAllPendingEvents() {
        synchronized (eventQueue) {
            SimEvent[] events = (SimEvent[]) eventQueue.toArray(new SimEvent[eventQueue.size()]);
            eventQueue.clear();
            return events;
        }
    }
}
