package lslplus.debug;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.LinkedList;

import lslplus.lsltest.TestEvents;
import lslplus.lsltest.TestManager;
import lslplus.lsltest.TestEvents.AllCompleteEvent;
import lslplus.lsltest.TestEvents.TestCompleteEvent;
import lslplus.lsltest.TestEvents.TestEvent;
import lslplus.lsltest.TestEvents.TestSuspendedEvent;
import lslplus.util.Util;

/**
 * Interact with a running test session.
 */
public class LslTestInteractor implements Runnable, Interactor {
    private LinkedList listeners = new LinkedList();
    private BufferedReader reader;
    private PrintStream writer;
    private String testDescriptor;
    private TestManager manager;
    private Thread thread;
    private boolean done = false;
    public LslTestInteractor(TestManager manager, String testDescriptor, InputStream in, 
            OutputStream out) {
        reader = new BufferedReader(new InputStreamReader(in));
        writer = new PrintStream(out);
        
        this.testDescriptor = testDescriptor;
        this.manager =  manager;
    }
    
    public void start() {
        if (done || thread != null && thread.isAlive()) return;
        writer.println(Util.URIEncode(testDescriptor));
        writer.flush();
        writer.println(Util.URIEncode("<exec-continue/>")); //$NON-NLS-1$
        writer.flush();
        thread = new Thread(this);
        thread.start();
    }
    
    public void continueExecution() {
        if (done || thread != null && thread.isAlive()) return;
        writer.println("<exec-continue/>");
        writer.flush();
        thread = new Thread(this);
        thread.start();
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
//        writer.println(Util.URIEncode(testDescriptor));
//        writer.flush();
//        writer.println(Util.URIEncode("<exec-continue/>")); //$NON-NLS-1$
//        writer.flush();
        String line = null;
        
        try {
            while ((line = reader.readLine()) != null) {
                Util.log("read:" + line);
                TestEvent event = TestEvents.fromXML(Util.URIDecode(line));
                
                // kludge for the mo'
                if (event instanceof TestCompleteEvent) {
                    manager.postTestResult(((TestCompleteEvent)event).getTestResult());
                    Util.log("writing: " + Util.URIEncode("<exec-continue/>"));
                    writer.println("<exec-continue/>");
                    writer.flush();
                } else if (event instanceof AllCompleteEvent) {
                    writer.println("quit");
                    writer.flush();
                    writer.close();
                    fireComplete();
                    // TODO: this is a place where a debug event would happen
                    return;
                } else if (event instanceof TestSuspendedEvent) {
                    // TODO: this is where we'd extract the debug info...
                    Util.log("hit a breakpoint... suspending!");
//                    writer.println("<exec-continue/>");
//                    writer.flush();
                    fireSuspended(((TestSuspendedEvent)event).getScriptState());
                    return;
                }
            }
        } catch (IOException e) {
            Util.log(e, e.getLocalizedMessage());
        }
    }
}
