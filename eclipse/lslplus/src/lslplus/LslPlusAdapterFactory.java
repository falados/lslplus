package lslplus;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

/**
 * An adapter factory that can create adapters for LslPlusElement objects.
 * It can adapt LslPlusElements into their underlying resource objects.
 * @author rgreayer
 *
 */
public class LslPlusAdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adaptableObject instanceof LslPlusElement &&
		    adapterType == IResource.class) {
			LslPlusElement e = (LslPlusElement) adaptableObject;
			return e.getResource();
		}
		return null;
	}

	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class
		};
	}

}
