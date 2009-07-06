package lslplus.cserver;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import lslplus.LslPlusElement;
import lslplus.LslProjectNature;
import lslplus.generated.Tuple2;
import lslplus.generated.Tuple3;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public class SourceListBuilder implements IResourceVisitor {
	private HashMap<String,String> moduleMap = new HashMap<String,String>();
	private HashMap<String,String> moduleNameToPath = new HashMap<String,String>();
	private HashMap<String,String> scriptMap = new HashMap<String,String>();
	private HashMap<String,String> scriptNameToPath = new HashMap<String,String>();
	private boolean optimize;
	
	public SourceListBuilder(boolean addOptimizeOption) {
	    optimize = addOptimizeOption;
	}
	
	public String getModulePath(String name) {
		return moduleNameToPath.get(name);
	}

	public String getScriptPath(String name) {
		return scriptNameToPath.get(name);
	}
	
	public boolean visit(IResource resource) throws CoreException {
		LslPlusElement element = (LslPlusElement) resource.getAdapter(LslPlusElement.class);
	
		if (element != null) {
			IFile f = (IFile) resource;
			IPath p = f.getLocation();
			IPath pp = f.getProjectRelativePath();
			String name = LslProjectNature.resourceToLslPlusName(resource);
			
			if (element.isModule()) {
			    moduleNameToPath.put(name,pp.toString());
			    moduleMap.put(name,p.toOSString());
			} else if (element.isScript()) {
				scriptNameToPath.put(name, pp.toString());
				scriptMap.put(name, p.toOSString());
			}
		}
		return true;
	}
	
	public Tuple3<Boolean, 
		LinkedList<Tuple2<String, String>>, 
		LinkedList<Tuple2<String,String>>> compilationInfo() {
		
		Tuple3<Boolean, 
		LinkedList<Tuple2<String, String>>, 
		LinkedList<Tuple2<String,String>>> result = 
			new Tuple3<Boolean, 
				LinkedList<Tuple2<String,String>>, 
				LinkedList<Tuple2<String,String>>>();
		
		result.el1 = optimize;
	
		LinkedList<Tuple2<String,String>> modules = new LinkedList<Tuple2<String,String>>();
		for (Map.Entry<String, String> entry : moduleMap.entrySet()) {
			Tuple2<String,String> tup = new Tuple2<String, String>();
			tup.el1 = entry.getKey();
			tup.el2 = entry.getValue();
			modules.add(tup);
		}
		
		result.el2 = modules;
		LinkedList<Tuple2<String,String>> scripts = new LinkedList<Tuple2<String,String>>();
		for (Map.Entry<String, String> entry : scriptMap.entrySet()) {
			Tuple2<String,String> tup = new Tuple2<String, String>();
			tup.el1 = entry.getKey();
			tup.el2 = entry.getValue();
			scripts.add(tup);
		}
		
		result.el3 = scripts;
		return result;
	}
}
