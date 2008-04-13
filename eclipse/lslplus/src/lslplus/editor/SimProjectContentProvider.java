package lslplus.editor;

import lslplus.sim.SimProject;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class SimProjectContentProvider implements ITreeContentProvider {
    private SimProject.WorldNode root;
    public SimProjectContentProvider(SimProject.WorldNode root) {
        this.root = root;
    }
    
    public Object[] getChildren(Object parentElement) {
        SimProject.Node node = (SimProject.Node) parentElement;
        
        return node.getChildren().toArray();
    }

    public Object getParent(Object element) {
        SimProject.Node node = (SimProject.Node) element;
        return node.getParent();
    }

    public boolean hasChildren(Object element) {
        SimProject.Node node = (SimProject.Node) element;
        return node.getChildren().size() > 0;
    }

    public Object[] getElements(Object inputElement) {
        return new SimProject.Node[] { root };
    }

    public void dispose() {
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }
}
