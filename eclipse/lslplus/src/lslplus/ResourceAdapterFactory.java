package lslplus;

import lslplus.lsltest.LslTestSuite;
import lslplus.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdapterFactory;

/**
 * And adapter factory that can transform IResource objects into various
 * different LSL elements types (LslElement objects, LslDerivedScript objects,
 * and LslTestSuite objects).
 * @author rgreayer
 *
 */
public class ResourceAdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adaptableObject instanceof IFile) {
			IFile f = (IFile) adaptableObject;
			String ext = f.getFileExtension();
			if (LslPlusElement.class.equals(adapterType) && ("lslp".equals(ext) //$NON-NLS-1$
			                                         || "lslm".equals(ext))) { //$NON-NLS-1$
				return new LslPlusElement(f);
			} else if ("lslp".equals(ext) && LslPlusScript.class.equals(adapterType)) { //$NON-NLS-1$
			    return new LslPlusScript(f);
			} else if ("lsl".equals(ext) && LslDerivedScript.class.equals(adapterType)) { //$NON-NLS-1$
				return new LslDerivedScript(f);
			} else if ("lslt".equals(ext) && LslTestSuite.class.equals(adapterType)) { //$NON-NLS-1$
				try {
					LslTestSuite suite = LslTestSuite.fromXml(f.getContents(), f);
					suite.setIResource(f);
					return suite;
				} catch (CoreException e) {
					Util.log(e, e.getLocalizedMessage());
					return null;
				}
			}
		}
		return null;
	}

	static private Class[] adapterList = {
		LslPlusElement.class,
		LslDerivedScript.class,
		LslTestSuite.class,
		LslPlusScript.class
	};
	
	public Class[] getAdapterList() {
		return adapterList;
	}

}
