package lslplus;

import java.util.HashSet;
import java.util.Iterator;

import lslplus.debug.LslSimProcess;
import lslplus.sim.SimStatuses;
import lslplus.simview.SimWatcherViewPart;
import lslplus.util.Util;

import org.eclipse.debug.core.DebugException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

public class SimManager {
    private HashSet listeners = new HashSet();
    private volatile boolean active  = false;
    private LslSimProcess process = null;
    public synchronized void addSimListener(SimListener listener) {
        this.listeners.add(listener);
    }
    public synchronized void removeSimListener(SimListener listener) {
        this.listeners.remove(listener);
    }

    public synchronized void simLaunched(LslSimProcess process) {
        active = true;
        this.process = process;
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(new Runnable() {
            public void run() { showSimWatcherInActivePage(findSimWatcherInActivePage());}
        });

        fireSimLaunched();
    }
    
    public synchronized void simStopped() {
        if (active) {
            active = false;
            process = null;
            fireSimEnded();
        }
    }

    public synchronized void stopSim() {
        if (process != null) {
            try {
                process.terminate();
            } catch (DebugException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
        
        simStopped();
    }
    
    public boolean canLaunch() {
        return !active;
    }

    private SimWatcherViewPart showSimWatcherInActivePage(SimWatcherViewPart simWatcher) {
        IWorkbenchPage page= null;
        try {
            try {
                page= LslPlusPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
            } catch (NullPointerException e) {
            }

            if (page == null)
                return null;

            if (simWatcher != null && simWatcher.isCreated()) {
                page.bringToTop(simWatcher);
                return simWatcher;
            }
            //  show the result view if it isn't shown yet
            return (SimWatcherViewPart) page.showView(SimWatcherViewPart.ID);
        } catch (PartInitException pie) {
            Util.log(pie, pie.getLocalizedMessage());
            return null;
        } finally{
            //restore focus stolen by the creation of the result view
//            if (page != null && activePart != null)
//                page.activate(activePart);
        }
    }

    private SimWatcherViewPart findSimWatcherInActivePage() {
        IWorkbenchPage page= LslPlusPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
        if (page == null)
            return null;
        return (SimWatcherViewPart) page.findView(SimWatcherViewPart.ID);
    }

    public synchronized void newLogMessages(SimStatuses.Message[] s) {
        for (Iterator i = listeners.iterator(); i.hasNext(); ) {
            SimListener l = (SimListener) i.next();
            l.newLogMessages(s);
        }
    }
    
    public boolean isSimActive() {
        return active;
    }
    
    private void fireSimLaunched() {
        for (Iterator i = listeners.iterator(); i.hasNext();) {
            SimListener listener = (SimListener) i.next();
            
            listener.simLaunched();
        }
    }
    
    private void fireSimEnded() {
        for (Iterator i = listeners.iterator(); i.hasNext(); ) {
            SimListener listener = (SimListener) i.next();
            listener.simEnded();
        }
    }
}
