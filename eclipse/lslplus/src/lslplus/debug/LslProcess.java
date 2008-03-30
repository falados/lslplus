/**
 * 
 */
package lslplus.debug;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import lslplus.LslPlusPlugin;
import lslplus.launching.Messages;
import lslplus.launching.TestLaunchDelegate.LineReaderMonitor;
import lslplus.launching.TestLaunchDelegate.StreamMonitor;
import lslplus.lsltest.TestManager;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;

public class LslProcess extends Thread implements IProcess {
	private Reader reader1;
	private Reader reader2;
	private ILaunch launch;
	private Thread processMonitor;
	private IStreamsProxy proxy = null;
	private Process p = null;
    private boolean terminated = false;
    private LineReaderMonitor inputMonitor;
    private LslTestInteractor interactor;
	public LslProcess(Process p, String descriptor, ILaunch launch, TestManager testManager) {
	    this.p = p;
	    processMonitor = new Thread() {
            public void run() {
                
                try {
                    LslProcess.this.p.waitFor();
                    synchronized (LslProcess.this) {
                        terminated = true;
                    }
                    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent(LslProcess.this, DebugEvent.TERMINATE)});
                } catch (InterruptedException e) {
                    LslProcess.this.p.destroy();
                    synchronized (LslProcess.this) { terminated = true; }
                    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent(LslProcess.this, DebugEvent.TERMINATE)});
                }
            }
	    };

		this.reader1 = new StringReader(""); //$NON-NLS-1$
		this.reader2 = new InputStreamReader(p.getErrorStream());
		this.launch = launch;
//		inputMonitor = new LineReaderMonitor(new BufferedReader(new InputStreamReader(p.getInputStream())), testManager);
//		inputMonitor.start();
		interactor = new LslTestInteractor(testManager,descriptor, p.getInputStream(), p.getOutputStream());
//		interactor.start();
//		processMonitor.start();
	}

	public void go() {
	    interactor.start();
	    processMonitor.start();
	}
	
    public String getAttribute(String key) {
		return null;
	}

	public int getExitValue() throws DebugException {
		try {
            return p.exitValue();
        } catch (IllegalThreadStateException e) {
            throw new DebugException(new Status(IStatus.ERROR, LslPlusPlugin.PLUGIN_ID, Messages.getString("TestLaunchDelegate.NOT_TERMINATED"))); //$NON-NLS-1$
        }
	}

	public String getLabel() {
		return Messages.getString("TestLaunchDelegate.TEST"); //$NON-NLS-1$
	}

	public ILaunch getLaunch() {
		return launch;
	}

	public IStreamsProxy getStreamsProxy() {
		if (proxy == null) {
		    
			final StreamMonitor errorStreamMonitor = new StreamMonitor(reader2);
			final StreamMonitor outputStreamMonitor = new StreamMonitor(reader1);
			proxy = new IStreamsProxy() {
				public IStreamMonitor getErrorStreamMonitor() {
					return errorStreamMonitor; 
				}

				public IStreamMonitor getOutputStreamMonitor() {
					return outputStreamMonitor;
				}

				public void write(String input) throws IOException {
				}
				
			};
		}
		return proxy;
	}

	public void setAttribute(String key, String value) {
	}

	public Object getAdapter(Class adapter) {
		return Platform.getAdapterManager().getAdapter(this, adapter);
	}

	public synchronized boolean canTerminate() {
		return !terminated;
	}

	public synchronized boolean isTerminated() {
		return terminated;
	}

	public void terminate() throws DebugException {
	    processMonitor.interrupt();
	}

    public void setThread(LslThread thread) {
        interactor.addListener(thread);
        thread.setInteractor(interactor);
    }
	
}