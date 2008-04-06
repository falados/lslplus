package lslplus.simview;

import java.util.Collections;
import java.util.LinkedList;

import lslplus.sim.SimStatuses;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class LogViewerContentProvider implements ITreeContentProvider {

    private java.util.LinkedList logMessages = new LinkedList();
    public Object[] getChildren(Object parentElement) {
        return null;
    }

    public Object getParent(Object element) {
        return null;
    }

    public boolean hasChildren(Object element) {
        return false;
    }

    public Object[] getElements(Object inputElement) {
        return logMessages.toArray();
    }

    public void dispose() {
        logMessages.clear();  // yes, this is unnecessary...
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }

    public void addMessages(SimStatuses.Message[] messages) {
        Collections.addAll(logMessages, messages);
        
    }

    public void clear() {
        logMessages.clear();
    }

}
