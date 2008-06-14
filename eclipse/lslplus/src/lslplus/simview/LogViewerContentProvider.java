package lslplus.simview;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import lslplus.sim.SimStatuses;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class LogViewerContentProvider implements ITreeContentProvider {
    private static final int MAX_MSGS = 250;
    private java.util.LinkedList logMessages = new LinkedList();
    private java.util.LinkedList archive = null;
    private HashSet archiveSet = new HashSet();
    public Object[] getChildren(Object parentElement) {
        if ("archive".equals(parentElement)) return archive.toArray();
        return null;
    }

    public Object getParent(Object element) {
        if (archiveSet.contains(element)) return "archive";
        return null;
    }

    public boolean hasChildren(Object element) {
        return "archive".equals(element);
    }

    public Object[] getElements(Object inputElement) {
        Object[] os = null; 
        if (archive == null) os = logMessages.toArray();
        else {
            os =logMessages.toArray(new Object[logMessages.size() + 1]);
            os[os.length - 1] = "archive";
        }
        return os;
    }

    public void dispose() {
        logMessages.clear();  // yes, this is unnecessary...
        archive = null;
        archiveSet.clear();
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }

    public void addMessages(SimStatuses.Message[] messages) {
        LinkedList temp = new LinkedList();
        Collections.addAll(temp, messages);
        logMessages.addAll(0, temp);
        
        if (logMessages.size() > MAX_MSGS) {
            if (archive == null) archive = new LinkedList();
            List newlyArchived =cut(logMessages, MAX_MSGS, logMessages.size());
            archive.addAll(0, newlyArchived);
            archiveSet.addAll(newlyArchived);
        }
    }

    private List cut(List l, int start, int end) {
        List l1 = new LinkedList();
        for (int i = start; i < end; i++) l1.add(l.remove(start));
        return l1;
    }
    
    public void clear() {
        logMessages.clear();
        archive = null;
        archiveSet.clear();
    }

}
