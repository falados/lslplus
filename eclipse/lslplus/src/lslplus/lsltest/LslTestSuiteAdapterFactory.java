package lslplus.lsltest;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

public class LslTestSuiteAdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (!(adaptableObject instanceof LslTestSuite) ||
		    !(IResource.class.equals(adapterType))) return null;
		return ((LslTestSuite)adaptableObject).getResource();
	}

	public Class[] getAdapterList() {
		return new Class[] { IResource.class };
	}

}
