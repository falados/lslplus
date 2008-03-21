package lslplus.launching;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashSet;
import java.util.Iterator;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.lsltest.LslTestSuite;
import lslplus.lsltest.TestManager;
import lslplus.lsltest.TestResult;
import lslplus.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.dialogs.MessageDialog;

public class TestLaunchDelegate implements ILaunchConfigurationDelegate {

	private static final String BLANK = ""; //$NON-NLS-1$
    private static final String UNIT_TESTER_EXE = "UnitTester.exe"; //$NON-NLS-1$

    private static class LineReaderMonitor extends Thread {
        private BufferedReader reader;
        private TestManager manager;
        public LineReaderMonitor(BufferedReader in, TestManager manager) {
            reader = in;
            this.manager = manager;
        }
        
        public void run() {
            String line;
            try {
                while ((line = reader.readLine()) != null) {
                    try {
                        TestResult result = TestResult.fromXML(line);
                        manager.postTestResult(result);
                    } catch (RuntimeException e) {
                        Util.error(Messages.getString("TestLaunchDelegate.BAD_DATA") + line); //$NON-NLS-1$
                    }
                }
            } catch (IOException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }
    
    private static class StreamMonitor implements IStreamMonitor {
		private HashSet listeners = new HashSet();
		private StringBuffer buf = new StringBuffer();
		private Reader reader;
		
		public StreamMonitor(Reader reader) {
			this.reader = reader;
			t.start();
		}
		
		public void addListener(IStreamListener listener) {
			listeners.add(listener);
		}

		public String getContents() {
			synchronized (buf) {
				String s = buf.toString();
				buf.setLength(0);
				return s;
			}
		}

		public void removeListener(IStreamListener listener) {
			listeners.remove(listener);
		}
		
		private Thread t = new Thread() {
			public void run() {
				char[] cbuf = new char[512];
				int count = 0;
				try {
					while ((count = reader.read(cbuf)) >= 0) {
						if (count > 0) {
							String s = new String(cbuf, 0, count);
							buf.append(cbuf, 0, count);
							notifyListeners(s);
						}
					}
				} catch (IOException e) {
				}
			}
		};
		
		private void notifyListeners(String text) {
			for (Iterator i = listeners.iterator(); i.hasNext(); ) {
				IStreamListener listener = (IStreamListener)i.next();
				listener.streamAppended(text, this);
			}
		}
	}
	
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		if (LslPlusPlugin.DEBUG) Util.log("launch!!!"); //$NON-NLS-1$

		String fullPath = configuration.getAttribute(LaunchLslTestShortcut.LC_RESOURCE_NAME, BLANK);
		Path path = new Path(fullPath);
		IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
		
		if (resource == null) {
		    MessageDialog.openError(null, Messages.getString("TestLaunchDelegate.TEST_NO_LONGER_EXISTS"),  //$NON-NLS-1$
		            Messages.getString("TestLaunchDelegate.REFERENCED_TEST_NO_LONGER_EXISTS")); //$NON-NLS-1$
		    return;
		}
		LslTestSuite suite = (LslTestSuite) resource.getAdapter(LslTestSuite.class);
		LslProjectNature nature = (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
		String sourceDescriptor = nature.projectSourceList();
		String suiteDescriptor = suite.toXml();
		String testDescriptor = "<test-descriptor>" + sourceDescriptor + suiteDescriptor + "</test-descriptor>";  //$NON-NLS-1$//$NON-NLS-2$
		Util.log(testDescriptor);
		TestManager testManager = LslPlusPlugin.getDefault().getTestManager();
		testManager.testLaunched(configuration, launch, suite.getTests().length);
		TestProcess p = new TestProcess(LslPlusPlugin.runExecutable(UNIT_TESTER_EXE, testDescriptor, false), launch, testManager);
		
        launch.addProcess(p);
	}

	public static class TestProcess extends Thread implements IProcess {
		private Reader reader1;
		private Reader reader2;
		private ILaunch launch;
		private Thread processMonitor;
		private IStreamsProxy proxy = null;
		private Process p = null;
        private boolean terminated = false;
		public TestProcess(Process p, ILaunch launch, TestManager testManager) {
		    this.p = p;
		    processMonitor = new Thread() {
                public void run() {
                    
                    try {
                        TestProcess.this.p.waitFor();
                        synchronized (TestProcess.this) {
                            terminated = true;
                        }
                        DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent(TestProcess.this, DebugEvent.TERMINATE)});
                    } catch (InterruptedException e) {
                        TestProcess.this.p.destroy();
                        synchronized (TestProcess.this) { terminated = true; }
                        DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent(TestProcess.this, DebugEvent.TERMINATE)});
                    }
                }
		    };

			this.reader1 = new StringReader(BLANK);
			this.reader2 = new InputStreamReader(p.getErrorStream());
			this.launch = launch;
			LineReaderMonitor inputMonitor = new LineReaderMonitor(new BufferedReader(new InputStreamReader(p.getInputStream())), testManager);
			inputMonitor.start();
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
		
	}
}
