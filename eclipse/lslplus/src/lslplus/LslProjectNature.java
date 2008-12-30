package lslplus;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import lslplus.language_metadata.LslParam;
import lslplus.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * Represents LSL Plus projects.  LSL modules, scripts and tests are intended
 * to be created within the context of an LSL Plus project, and and LSL Plus
 * project is simply a basic Eclipse project with an LSL Project Nature associated
 * with it.
 * @author rgreayer
 *
 */
public class LslProjectNature implements IProjectNature, IResourceChangeListener {
	private class DeltaVisitor implements IResourceDeltaVisitor {
		private boolean lslContentChange = false;
		private LinkedList newDerivedResources = new LinkedList();
		public List getNewDerivedResources() {
			return newDerivedResources;
		}
		
		public boolean hasLslContentChange() {
			return lslContentChange;
		}
		
		public boolean visit(IResourceDelta delta) throws CoreException {
			IResource resource = delta.getResource();
			if (resource != null) {
				if (resource.getProject() != null && resource.getProject() != project) {
					return false; // don't continue down this branch...
				}

				LslPlusElement element = (LslPlusElement) resource.getAdapter(LslPlusElement.class);
				if (element != null) {
					if (delta.getKind() == IResourceDelta.ADDED ||
					    delta.getKind() == IResourceDelta.REMOVED) {
						lslContentChange = true;
					} else if (delta.getKind() == IResourceDelta.CHANGED &&
							   ((delta.getFlags() & IResourceDelta.REPLACED) != 0 ||
							    (delta.getFlags() & IResourceDelta.CONTENT) != 0)) {
						lslContentChange = true;
					}
				} else {
					LslDerivedScript script = (LslDerivedScript) resource.getAdapter(LslDerivedScript.class);
					if (script != null && delta.getKind() == IResourceDelta.ADDED) {
						newDerivedResources.add(resource);
					}
				}
			}
			return true;
		}
	}
	
	public static class EntryPointDefinition {
		public String name;
		public LslParam[] params;
		public String returnType;
	}
	
	public static class NameTypePair {
	    private String name;
	    private String type;
	    public String getName() { return name; }
	    public String getType() { return type; }
	}
	
	public static class ErrorLocation {
		public int columnEnd;
		public int columnStart;
		public int lineEnd;
		public int lineStart;
	}
	
	public static class Item {
		public EntryPointDefinition[] entryPoints;
		public String name;
		public ItemStatus status;
		private NameTypePair[] globals;
        public void setGlobals(NameTypePair[] globals) {
            this.globals = globals;
        }
        public NameTypePair[] getGlobals() {
            if (globals == null) {
                globals = new NameTypePair[0];
            }
            return globals;
        }
	}
	
	public static class ItemStatus {
		//public ErrorLocation errLoc = null;
		//public String msg = null;
	    public ItemError[] errs = null;
		public boolean ok = true;
	}
	
	public static class ItemError {
	    public ErrorLocation errLoc = null;
	    public String msg = null;
	}
	
	private class SourceListBuilder implements IResourceVisitor {
		private static final String IDENTIFIER_BEGIN = "<identifier>"; //$NON-NLS-1$
		private static final String IDENTIFIER_END = "</identifier>"; //$NON-NLS-1$
		private static final String ITEM_BEGIN = "<item>"; //$NON-NLS-1$
		private static final String ITEM_END = "</item>"; //$NON-NLS-1$
		private static final String MODULES_BEGIN = "<modules>"; //$NON-NLS-1$
		private static final String MODULES_END = "</modules>"; //$NON-NLS-1$
		private static final String PATH_BEGIN = "<path>"; //$NON-NLS-1$
		private static final String PATH_END = "</path>"; //$NON-NLS-1$
		private static final String SCRIPTS_BEGIN = "<scripts>"; //$NON-NLS-1$
		private static final String SCRIPTS_END = "</scripts>"; //$NON-NLS-1$
		private static final String SOURCE_LIST_BEGIN = "<source_files>"; //$NON-NLS-1$
		private static final String SOURCE_LIST_END = "</source_files>"; //$NON-NLS-1$
		private HashMap moduleMap = new HashMap();
		private HashMap moduleNameToPath = new HashMap();
		private HashMap scriptMap = new HashMap();
		private HashMap scriptNameToPath = new HashMap();
		private void buildItemList(StringBuilder buf, Map m) {
			for (Iterator i = m.entrySet().iterator(); i.hasNext();) {
				Map.Entry e = (Map.Entry) i.next();
				buf.append(ITEM_BEGIN);
				buf.append(IDENTIFIER_BEGIN);
				buf.append(e.getKey());
				buf.append(IDENTIFIER_END);
				buf.append(PATH_BEGIN);
				buf.append(e.getValue());
				buf.append(PATH_END);
				buf.append(ITEM_END);
			}
		}
		
		public String getModulePath(String name) {
			return (String) moduleNameToPath.get(name);
		}

		public String getScriptPath(String name) {
			return (String) scriptNameToPath.get(name);
		}
		
		public boolean visit(IResource resource) throws CoreException {
			LslPlusElement element = (LslPlusElement) resource.getAdapter(LslPlusElement.class);
		
			if (element != null) {
				IFile f = (IFile) resource;
				IPath p = f.getLocation();
				IPath pp = f.getProjectRelativePath();
				String name = resourceToLslPlusName(resource);
				
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
		public String xmlDescriptor() {
			StringBuilder buf = new StringBuilder();
			buf.append(SOURCE_LIST_BEGIN);
			buf.append(MODULES_BEGIN);
			buildItemList(buf, moduleMap);
			buf.append(MODULES_END);
			buf.append(SCRIPTS_BEGIN);
			buildItemList(buf, scriptMap);
			buf.append(SCRIPTS_END);
			buf.append(SOURCE_LIST_END);
			return buf.toString();
		}
	}
	
	public static class Summary {
		private Item[] modules; 
		private Item[] scripts;
		public Item[] getModules() {
			return modules;
		}
		public Item[] getScripts() {
			return scripts;
		}
		public void setModules(Item[] modules) {
			this.modules = modules;
		}
		public void setScripts(Item[] scripts) {
			this.scripts = scripts;
		}
	}
	
	public static String ID = "lslplus.lslPlusNature"; //$NON-NLS-1$
	
	private static final String LSLPLUS = "lslplus"; //$NON-NLS-1$

	private static final String LSLPLUS_PROBLEM = "lslplus.problem"; //$NON-NLS-1$

	private Map entryPoints;
	private Map globalVariables;
	private IProject project;
	
	private Summary summary;

	public LslProjectNature() {
		if (LslPlusPlugin.DEBUG) Util.log("creating project nature"); //$NON-NLS-1$
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
	}
	
	private synchronized void checkForErrors() {
		
		try {
			final SourceListBuilder builder = new SourceListBuilder();
			project.accept(builder);
			String descriptor = builder.xmlDescriptor();
			if (LslPlusPlugin.DEBUG) Util.log("descriptor: " + descriptor); //$NON-NLS-1$
			String result = LslPlusPlugin.runTask("Compiler", descriptor); //$NON-NLS-1$
			if (LslPlusPlugin.DEBUG) Util.log("result: " + result); //$NON-NLS-1$
			if (result == null) return;
			XStream xstream = new XStream(new DomDriver());

			xstream.alias("summary", Summary.class); //$NON-NLS-1$
			xstream.alias("item", Item.class); //$NON-NLS-1$
			xstream.alias("entryPoint", EntryPointDefinition.class); //$NON-NLS-1$
			xstream.alias("param", LslParam.class); //$NON-NLS-1$
			xstream.alias("global", NameTypePair.class); //$NON-NLS-1$
			xstream.alias("itemError", ItemError.class); //$NON-NLS-1$
			summary = (Summary) xstream.fromXML(result);
			final HashMap map = new HashMap();
			synchronized (this) {
				entryPoints = new HashMap();
				globalVariables = new HashMap();
			    for (int i = 0; i < summary.getModules().length; i++) {
			    	Item item = summary.getModules()[i];
			    	map.put(builder.getModulePath(item.name), item.status);
			    	if (item.status.ok) {
			    		entryPoints.put(item.name, item.entryPoints);
			    		globalVariables.put(item.name, item.getGlobals());
			    	}
			    }
			    
			    for (int i = 0; i < summary.getScripts().length; i++) {
			    	Item item = summary.getScripts()[i];
			    	map.put(builder.getScriptPath(summary.getScripts()[i].name), summary.getScripts()[i].status);
			    	if (item.status.ok) {
			    		entryPoints.put(item.name, item.entryPoints);
			    		globalVariables.put(item.name, item.getGlobals());
			    	}
			    }
			}

			project.accept(new IResourceVisitor() {
				public boolean visit(IResource resource) throws CoreException {
					hasError(resource, map);
					return true;
				}
			});

		} catch (CoreException e) {
			Util.log(e, e.getLocalizedMessage());
		} catch (Exception e) {
			Util.log(e, e.getLocalizedMessage());
		}
		
		LslPlusPlugin.getDefault().errorStatusChanged();
		
        WorkspaceJob job = new WorkspaceJob(Messages.ProjectNature_REFRESH) {

            public IStatus runInWorkspace(IProgressMonitor monitor)
                    throws CoreException {
                project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
                return new Status(IStatus.OK, "lslplus", Messages.ProjectNature_REFRESHED_OK); //$NON-NLS-1$
            }
            
        };
        
        job.schedule(100);
	}
	
	public void configure() throws CoreException {
	}
	
	public void deconfigure() throws CoreException {
	}
	
	public synchronized String[] getEntryPointNames(String fileName) {
		if (fileName == null) return null;
		EntryPointDefinition[] eps = (EntryPointDefinition[]) entryPoints.get(fileName);
		if (eps == null) return new String[0];
		String[] paths = new String[eps.length];
		for (int i = 0; i < eps.length; i++) paths[i] = eps[i].name;
		Arrays.sort(paths);
		return paths;
	}
	
	
	public LslParam[] getParams(String fileName, final String entryPointName) {
		EntryPointDefinition[] eps = (EntryPointDefinition[]) entryPoints.get(fileName);
		if (eps == null) return null;
		
		EntryPointDefinition ep = (EntryPointDefinition) Util.find(new Util.Predicate() {
			public boolean test(Object o) { return ((EntryPointDefinition)o).name.equals(entryPointName); }
		}, eps);
		if (ep == null) return null;
		return ep.params;
	}
	
	public String getReturnType(String fileName, final String entryPointName) {
		EntryPointDefinition[] eps = (EntryPointDefinition[]) entryPoints.get(fileName);
		if (eps == null) return null;
		EntryPointDefinition ep = (EntryPointDefinition) Util.find(new Util.Predicate() {
			public boolean test(Object o) { return ((EntryPointDefinition)o).name.equals(entryPointName); }
		}, eps);
	    if (ep == null) return null;
	    return ep.returnType;
	}
	
	public synchronized String[] getLslFiles() {
		if (entryPoints == null) return new String[0];
		String[] files = (String[]) entryPoints.keySet().toArray(new String[entryPoints.size()]);
		Arrays.sort(files);
		return files;
	}
	
	public String[] getLslScripts() {
	    if (entryPoints == null) return new String[0];
	    ArrayList l = new ArrayList();
	    
	    synchronized (this) {
	        for (Iterator  i = entryPoints.keySet().iterator(); i.hasNext();) {
	            String name = (String) i.next();
	            
	            if (name.endsWith(".lslp")) { //$NON-NLS-1$
	                l.add(name);
	            }
	        }
	    }
	    
	    String[] scripts = (String[]) l.toArray(new String[l.size()]);
	    Arrays.sort(scripts);
	    return scripts;
	}
	
    public List getLslModules() {
        ArrayList l = new ArrayList();
        if (entryPoints == null) return l;
        
        synchronized (this) {
            for (Iterator  i = entryPoints.keySet().iterator(); i.hasNext();) {
                String name = (String) i.next();
                
                if (name.endsWith(".lslm")) { //$NON-NLS-1$
                    l.add(name);
                }
            }
        }
        
        Collections.sort(l);
        return l;
    }
    
	
	public IProject getProject() {
	    return project;
	}
	
	public Summary getSummary() {
		return summary;
	}
	
	private void hasError(IResource resource, Map summary) {
		if (resource instanceof IFile) {
			IFile f = (IFile) resource;
			if (f.exists()) {
				try {
					
					resource.deleteMarkers(LSLPLUS_PROBLEM, true, IResource.DEPTH_ONE);
					String key = resource.getProjectRelativePath().toString();
					
					ItemStatus status = (ItemStatus) summary.get(key);
					if (status != null && !status.ok) {
					    for (int j = 0; j < status.errs.length; j++) {
                            ItemError err = status.errs[j];
                            IMarker i = resource.createMarker(LSLPLUS_PROBLEM);
                            i.setAttribute(IMarker.MESSAGE, err.msg);
                            i.setAttribute(IMarker.SEVERITY,
                                    IMarker.SEVERITY_ERROR);
                            if (err.errLoc != null) {
                                int lineOffset0 = err.errLoc.lineStart - 1;
                                int lineOffset1 = err.errLoc.lineEnd - 1;
                                i.setAttribute(IMarker.LINE_NUMBER,
                                        err.errLoc.lineStart);
                                int[] offsets = Util.findOffsetsFor(new int[] {
                                        lineOffset0, lineOffset1 }, new int[] {
                                        err.errLoc.columnStart - 1,
                                        err.errLoc.columnEnd - 1 }, f);
                                if (offsets != null) {
                                    if (offsets[0] == offsets[1])
                                        offsets[1]++;
                                    i.setAttribute(IMarker.CHAR_START,
                                            offsets[0]);
                                    i
                                            .setAttribute(IMarker.CHAR_END,
                                                    offsets[1]);
                                }
                            }
                        }
					    
                        Util.log("Marked " + key);
					}
				} catch (CoreException e) {
					Util.log(e, "error reading file"); //$NON-NLS-1$
				}
			}
		}
	}
	
	public void resourceChanged(IResourceChangeEvent event) {
	    Util.log("resource changed!");
		final IResourceDelta delta = event.getDelta();

		DeltaVisitor dv = new DeltaVisitor();
		
		boolean checkErrors = false;
		try {
		    delta.accept(dv);
		    checkErrors = dv.hasLslContentChange();
		} catch (CoreException e) {
			checkErrors = true;
			Util.log(e, e.getLocalizedMessage());
		}
		
		if (checkErrors) {
		    Util.log("check errors!");
			WorkspaceJob job = new WorkspaceJob("EvaluateErrors") { //$NON-NLS-1$

				public IStatus runInWorkspace(IProgressMonitor monitor) {
					checkForErrors();
					return new Status(IStatus.OK,LSLPLUS, Messages.ProjectNature_OK);
				}
				
			};
			
			job.schedule();
		}
		
		final List newDerivedResources = dv.getNewDerivedResources();
		if (newDerivedResources != null && newDerivedResources.size() > 0) {
			WorkspaceJob job = new WorkspaceJob("MarkDerived") { //$NON-NLS-1$

				public IStatus runInWorkspace(IProgressMonitor monitor)
						throws CoreException {
					for (Iterator i = newDerivedResources.iterator();
					     i.hasNext(); ) {
						IResource r = (IResource) i.next();
						r.setDerived(true);
					}
					
					return new Status(IStatus.OK, LSLPLUS, Messages.ProjectNature_MARK_DERIVED_COMPLETE);
				}
				
			};
			
			job.schedule();
		}
	}
	
	public void setProject(IProject project) {
		this.project = project;
		checkForErrors();
	}

	/**
	 * Gets the XML source list descriptor for the project.
	 * @return the LSL source list XML descriptor for the project
	 * @throws CoreException 
	 */
	public String projectSourceList() throws CoreException {
        final SourceListBuilder builder = new SourceListBuilder();
        project.accept(builder);
        return builder.xmlDescriptor();
	}

    public synchronized NameTypePair[] getGlobalVariables(String fileName) {
        return (NameTypePair[]) globalVariables.get(fileName);
    }
    
    public static String resourceToLslPlusName(IResource r) {
        return r.getProjectRelativePath().toString().replace('/', '.');
    }
}
