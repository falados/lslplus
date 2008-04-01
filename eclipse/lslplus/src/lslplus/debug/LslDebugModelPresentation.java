package lslplus.debug;

import java.util.LinkedList;

import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.ISourcePresentation;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorInput;

public class LslDebugModelPresentation implements IDebugModelPresentation {
    private ISourcePresentation presentation = new LslSourceLocator();
    private LinkedList listeners = new LinkedList();
    public void computeDetail(IValue value, IValueDetailListener listener) {
        listener.detailComputed(value, null);
    }

    public Image getImage(Object element) {
        return null;
    }

    public String getText(Object element) {
        return null;
    }

    public void setAttribute(String attribute, Object value) {
    }

    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    public void dispose() {
    }

    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    public String getEditorId(IEditorInput input, Object element) {
        return presentation.getEditorId(input, element);
    }

    public IEditorInput getEditorInput(Object element) {
        return presentation.getEditorInput(element);
    }

}
