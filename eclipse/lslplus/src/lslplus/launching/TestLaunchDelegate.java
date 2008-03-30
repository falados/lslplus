package lslplus.launching;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashSet;
import java.util.Iterator;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.debug.LslDebugTarget;
import lslplus.debug.LslProcess;
import lslplus.debug.LslSourceLocator;
import lslplus.lsltest.LslTestSuite;
import lslplus.lsltest.TestManager;
import lslplus.lsltest.TestResult;
import lslplus.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.jface.dialogs.MessageDialog;

public class TestLaunchDelegate implements ILaunchConfigurationDelegate {

	static final String BLANK = ""; //$NON-NLS-1$
    private static final String UNIT_TESTER_EXE = "UnitTester2.exe"; //$NON-NLS-1$

    public static class LineReaderMonitor extends Thread {
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
    
    public static class StreamMonitor implements IStreamMonitor {
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
		LslProcess p = // new LslProcess(LslPlusPlugin.runExecutable(UNIT_TESTER_EXE, testDescriptor, false), launch, testManager);
		                new LslProcess(LslPlusPlugin.launchExecutable(UNIT_TESTER_EXE, false), 
		                        testDescriptor, launch, testManager);
		LslDebugTarget target = new LslDebugTarget("lslplus-test", launch, p); //$NON-NLS-1$
		launch.addDebugTarget(target);
        launch.addProcess(p);
        launch.setSourceLocator(new LslSourceLocator());
        p.go();
	}
}
