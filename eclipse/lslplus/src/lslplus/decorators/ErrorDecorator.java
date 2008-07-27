package lslplus.decorators;

import java.util.Iterator;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.util.Util;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;

/**
 * Decorate the navigator with error indications.
 */
public class ErrorDecorator implements ILightweightLabelDecorator {
    private static final String ICON_PATH = "icons/error_decorator.gif"; //NON-NLS-1 //$NON-NLS-1$

    private LinkedList labelProviderListeners = new LinkedList();

	private ImageDescriptor descriptor;
	
	public ErrorDecorator() {
		LslPlusPlugin.getDefault().setErrorDecorator(this);
		descriptor = Util.findDescriptor(ICON_PATH);
	}

	public void decorate(Object element, IDecoration decoration) {
		if (! (element instanceof IResource)) return;
		IResource resource = (IResource) element;
		if (!resource.exists()) return;
		IProject project = resource.getProject();
		if (project == null) {
			Util.error(Messages.getString("ErrorDecorator.PROJECT_FOR") + resource.getName() + Messages.getString("ErrorDecorator.IS_NULL")); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}
		try {
		    if (!project.isOpen()) return;
			project.open(null);
			if (project.hasNature(LslProjectNature.ID)) { 
				LslProjectNature nature = (LslProjectNature) project.getNature(LslProjectNature.ID);
				
				if (nature == null) return;

				IMarker[] m = resource.findMarkers("lslplus.problem", true, IResource.DEPTH_INFINITE); //$NON-NLS-1$
				
				if (m == null || m.length == 0) return;
			} else {
				return;
			}
		} catch (CoreException e) {
			Util.log(e,"exception caught trying to determine project nature!"); //$NON-NLS-1$
			return;
		}

		decoration.addOverlay(descriptor,IDecoration.BOTTOM_LEFT);
	}

	public void addListener(ILabelProviderListener listener) {
		labelProviderListeners.add(listener);
	}

	public void dispose() {
	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		labelProviderListeners.remove(listener);
	}
	
	public void errorStatusChanged() {
		Iterator i = labelProviderListeners.iterator();
		
		while (i.hasNext()) {
			ILabelProviderListener listener = (ILabelProviderListener) i.next();
			listener.labelProviderChanged(new LabelProviderChangedEvent(this));
		}
	}
}