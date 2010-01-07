package lslplus;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import lslplus.cserver.CompilationServer;
import lslplus.cserver.CompilationServer.Result;
import lslplus.generated.CompilationCommand_Init;
import lslplus.generated.CompilationCommand_RemoveScript;
import lslplus.generated.CompilationCommand_UpdateScript;
import lslplus.generated.CompilationResponse;
import lslplus.generated.CompilationResponse_FullSourceValidation;
import lslplus.generated.CompilationStatus;
import lslplus.generated.CompilationStatus_CompilationStatus;
import lslplus.generated.EPSummary;
import lslplus.generated.EPSummary_EPSummary;
import lslplus.generated.Either_Left;
import lslplus.generated.Either_Right;
import lslplus.generated.ErrInfo;
import lslplus.generated.ErrInfo_ErrInfo;
import lslplus.generated.GlobalSummary;
import lslplus.generated.LSLType;
import lslplus.generated.Maybe_Just;
import lslplus.generated.TextLocation;
import lslplus.generated.TextLocation_TextLocation;
import lslplus.generated.Tuple2;
import lslplus.generated.Tuple3;
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

/**
 * Represents LSL Plus projects.  LSL modules, scripts and tests are intended
 * to be created within the context of an LSL Plus project, and and LSL Plus
 * project is simply a basic Eclipse project with an LSL Project Nature associated
 * with it.
 * @author rgreayer
 *
 */
public class LslProjectNature implements IProjectNature, IResourceChangeListener {
	public static final String OPTIMIZE = "optimize"; //$NON-NLS-1$

	public static interface RecompileListener {
		public void recompile();
	}
	
    private static class BetterDeltaVisitor implements IResourceDeltaVisitor {
		private boolean recompileAll = false;
		private LinkedList<IResource> newDerivedResources = new LinkedList<IResource>();
		private LinkedList<LslPlusElement> addsAndUpdates = new LinkedList<LslPlusElement>();
		private LinkedList<LslPlusElement> removals = new LinkedList<LslPlusElement>();
		private IProject project;
		
		public BetterDeltaVisitor(IProject p) {
			project = p;
		}
		
		public List<LslPlusElement> getAddsAndUpdates() {
			return addsAndUpdates;
		}
		
		public List<LslPlusElement> getRemovals() {
			return removals;
		}
		
		public List<IResource> getNewDerivedResources() {
			return newDerivedResources;
		}
		
		public boolean isRecompileAll() {
			return recompileAll;
		}
		
		public boolean visit(IResourceDelta delta) throws CoreException {
			IResource resource = delta.getResource();
			if (resource != null) {
				if (resource.getProject() != null && !isRelevant(resource.getProject())) {
					return false; // don't continue down this branch...
				}

				LslPlusElement element = (LslPlusElement) resource.getAdapter(LslPlusElement.class);
				
				if (element != null) {
					if (element.isModule()) {
						recompileAll = recompileAll || 
							(delta.getKind() == IResourceDelta.ADDED ||
						     delta.getKind() == IResourceDelta.REMOVED ||
						     (delta.getKind() == IResourceDelta.CHANGED &&
						      ((delta.getFlags() & IResourceDelta.REPLACED) != 0 ||
						       (delta.getFlags() & IResourceDelta.CONTENT) != 0)));
					}  else {
						if (delta.getKind() == IResourceDelta.ADDED ||
							(delta.getKind() == IResourceDelta.CHANGED &&
									((delta.getFlags() & IResourceDelta.REPLACED) != 0 ||
										    (delta.getFlags() & IResourceDelta.CONTENT) != 0))) {
							addsAndUpdates.add(element);
						} else if (delta.getKind() == IResourceDelta.REMOVED) {
							removals.add(element);
						}
						
					}
				} else {
					LslDerivedScript script = 
						(LslDerivedScript) resource.getAdapter(LslDerivedScript.class);
					if (script != null && delta.getKind() == IResourceDelta.ADDED) {
						newDerivedResources.add(resource);
					} else if (script == null) {
						IProject p = (IProject) resource.getAdapter(IProject.class);
						if (p != null && delta.getKind() == IResourceDelta.CHANGED &&
								(delta.getFlags() & IResourceDelta.DESCRIPTION) != 0)  {
						    recompileAll = true;
						}
					}
				}
			}
			return true;
		}
		
	    @SuppressWarnings("nls")
		private boolean isRelevant(IProject proj) {
	    	if (proj == project) return true;
	    	try {
				for (IProject p : project.getReferencedProjects()) {
					if (p == proj) return true;
				}
				return false;
			} catch (CoreException e) {
				Util.error(e,"problem getting referenced projects");
				return false;
			}

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
		private HashMap<String,String> moduleMap = new HashMap<String,String>();
		private HashMap<String,String> moduleNameToPath = new HashMap<String,String>();
		private HashMap<String,String> scriptMap = new HashMap<String,String>();
		private HashMap<String,String> scriptNameToPath = new HashMap<String,String>();
		private boolean optimize;
		private boolean modulesOnly = true;
		
		public SourceListBuilder(boolean addOptimizeOption) {
		    optimize = addOptimizeOption;
		}
		
		private void buildItemList(StringBuilder buf, Map<String,String> m) {
			//for (Iterator i = m.entrySet().iterator(); i.hasNext();) {
            for (Map.Entry<String, String> e : m.entrySet()) {
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
				String name = resourceToLslPlusName(resource);
				
				if (element.isModule()) {
				    moduleNameToPath.put(name,pp.toString());
				    moduleMap.put(name,p.toOSString());
				} else if (element.isScript() && !modulesOnly) {
					scriptNameToPath.put(name, pp.toString());
					scriptMap.put(name, p.toOSString());
				}
			}
			return true;
		}
		public String xmlDescriptor() {
			StringBuilder buf = new StringBuilder();
			buf.append(SOURCE_LIST_BEGIN);
			if (optimize) buf.append("<optimize>true</optimize>"); //$NON-NLS-1$
			buf.append(MODULES_BEGIN);
			buildItemList(buf, moduleMap);
			buf.append(MODULES_END);
			buf.append(SCRIPTS_BEGIN);
			buildItemList(buf, scriptMap);
			buf.append(SCRIPTS_END);
			buf.append(SOURCE_LIST_END);
			return buf.toString();
		}
		
		public void setModulesOnly(boolean m) {
			this.modulesOnly = m;
		}
	}
	
	public static class Summary {
//		private Item[] modules; 
//		private Item[] scripts;
		private LinkedList<CompilationStatus> modules;
		private LinkedList<CompilationStatus> scripts;
		
		public LinkedList<CompilationStatus> getModules() {
			return modules;
		}
		public LinkedList<CompilationStatus> getScripts() {
			return scripts;
		}
		public void setModules(LinkedList<CompilationStatus> modules) {
			this.modules = modules;
		}
		public void setScripts(LinkedList<CompilationStatus> scripts) {
			this.scripts = scripts;
		}
	}
	
	public static String ID = "lslplus.lslPlusNature"; //$NON-NLS-1$
	
	private static final String LSLPLUS = "lslplus"; //$NON-NLS-1$

	private static final String LSLPLUS_PROBLEM = "lslplus.problem"; //$NON-NLS-1$

	private HashSet<RecompileListener> recompListeners = new HashSet<RecompileListener>();
	private Map<String,LinkedList<EPSummary>> entryPoints;
	private Map<String,LinkedList<GlobalSummary>> globalVariables;
	private IProject project;
	
	private Summary summary;

	private CompilationServer cserver;
	public LslProjectNature() {
		if (LslPlusPlugin.DEBUG) Util.log("creating project nature"); //$NON-NLS-1$
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
		cserver = new CompilationServer();
	}
	
	public CompilationServer getCompilationServer() {
	    return cserver;
	}
	
	private synchronized void checkForErrors(boolean recompileAll, List<LslPlusElement> scriptChanges,
			List<LslPlusElement> scriptRemovals) {
		
		try {
			if (scriptRemovals != null) {
				for (LslPlusElement e : scriptRemovals) {
					String name = resourceToLslPlusName(e.getResource());
					String path = e.getResource().getLocation().toOSString();
					CompilationCommand_RemoveScript cmd = new CompilationCommand_RemoveScript();
					cmd.el1 = new Tuple2<String, String>();
					cmd.el1.el1 = name;
					cmd.el1.el2 = path;
					Result r = cserver.execute(cmd);
					r.get(); // don't care about response...
				}
			}
			
			CompilationResponse response = null;
			
			if (recompileAll || (scriptChanges != null && scriptChanges.size() != 0)) {
				IProject[] ps = project.getReferencedProjects();
				boolean optimize = 
					LslPlusPlugin.getDefault().getPreferenceStore().getBoolean(OPTIMIZE);
				final lslplus.cserver.SourceListBuilder builder = 
					new lslplus.cserver.SourceListBuilder(optimize);
				
				for (IProject p : ps) {
			        if (p.hasNature(LslProjectNature.ID)) {
			        	p.accept(builder);
			        }
				}
				
				builder.setModulesOnly(false);
				project.accept(builder);
				if (recompileAll) {
					Tuple3<Boolean, LinkedList<Tuple2<String, String>>, 
						LinkedList<Tuple2<String, String>>> cinfo = builder
							.compilationInfo();
					CompilationCommand_Init cc = new CompilationCommand_Init();
					cc.el1 = cinfo;

					Result r = cserver.execute(cc);

					response = r.get();
				} else {
					for (LslPlusElement e : scriptChanges) {
						CompilationCommand_UpdateScript cmd = new CompilationCommand_UpdateScript();
						cmd.el1 = new Tuple2<String, String>();
						cmd.el1.el1 = resourceToLslPlusName(e.getResource());
						cmd.el1.el2 = e.getResource().getLocation().toOSString();
						Result r = cserver.execute(cmd);
						response = r.get(); // wait for response... only care about last one!
					}
				}
				
				if (!(response instanceof CompilationResponse_FullSourceValidation)) {
					Util.error("unexpected response from compilation server"); //$NON-NLS-1$
					return;
				}
				CompilationResponse_FullSourceValidation validation = 
					(CompilationResponse_FullSourceValidation) response;
				summary = new Summary();
				summary.setModules(validation.el1.el1);
				summary.setScripts(validation.el1.el2);
				final HashMap<String, LinkedList<ErrInfo>> map = new HashMap<String, LinkedList<ErrInfo>>();
				synchronized (this) {
					entryPoints = new HashMap<String, LinkedList<EPSummary>>();
					globalVariables = new HashMap<String, LinkedList<GlobalSummary>>();
					for (CompilationStatus status0 : summary.getModules()) {
						CompilationStatus_CompilationStatus status = (CompilationStatus_CompilationStatus) status0;

						map.put(builder.getModulePath(status.csName),
										(status.csInfo instanceof Either_Right) ? new LinkedList<ErrInfo>()
												: ((Either_Left<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo).el1);
						if (status.csInfo instanceof Either_Right) {
							Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>> info = (Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo;

							entryPoints.put(status.csName, info.el1.el2);
							globalVariables.put(status.csName, info.el1.el1);
						}
					}

					for (CompilationStatus status0 : summary.getScripts()) {
						CompilationStatus_CompilationStatus status = (CompilationStatus_CompilationStatus) status0;

						map.put(builder.getScriptPath(status.csName),
										(status.csInfo instanceof Either_Right) ? new LinkedList<ErrInfo>()
												: ((Either_Left<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo).el1);
						if (status.csInfo instanceof Either_Right) {
							Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>> info = (Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo;

							entryPoints.put(status.csName, info.el1.el2);
							globalVariables.put(status.csName, info.el1.el1);
						}
					}
				}
				project.accept(new IResourceVisitor() {
					public boolean visit(IResource resource)
							throws CoreException {
						hasError(resource, map);
						return true;
					}
				});
			}
		} catch (CoreException e) {
			Util.error(e, e.getLocalizedMessage());
		} catch (Exception e) {
			Util.error(e, e.getLocalizedMessage());
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
		LinkedList<EPSummary> eps = entryPoints.get(fileName);
		//EntryPointDefinition[] eps = (EntryPointDefinition[]) entryPoints.get(fileName);
		if (eps == null) return new String[0];
		String[] paths = new String[eps.size()];
		int i = 0;
		for (EPSummary s : eps) paths[i++] = ((EPSummary_EPSummary)s).epName;
		Arrays.sort(paths);
		return paths;
	}
	
	
	public LinkedList<Tuple2<String,LSLType>> getParams(String fileName, final String entryPointName) {
		LinkedList<EPSummary> eps = entryPoints.get(fileName);
		if (eps == null) return null;
		
		EPSummary_EPSummary ep = null;
		for (EPSummary s0 : eps) {
			EPSummary_EPSummary s = (EPSummary_EPSummary) s0;
			
			if (s.epName.equals(entryPointName)) {
				ep = s;
				break;
			}
		}
		if (ep == null) return null;
		return ep.epParams;
	}
	
	public LSLType getReturnType(String fileName, final String entryPointName) {
		LinkedList<EPSummary> eps = entryPoints.get(fileName);
		if (eps == null) return null;
		
		EPSummary_EPSummary ep = null;
		for (EPSummary s0 : eps) {
			EPSummary_EPSummary s = (EPSummary_EPSummary) s0;
			
			if (s.epName.equals(entryPointName)) {
				ep = s;
				break;
			}
		}
		if (ep == null) return null;
	    return ep.epType;
	}
	
	public synchronized String[] getLslFiles() {
		if (entryPoints == null) return new String[0];
		String[] files = entryPoints.keySet().toArray(new String[entryPoints.size()]);
		Arrays.sort(files);
		return files;
	}
	
	public String[] getLslScripts() {
	    if (entryPoints == null) return new String[0];
	    ArrayList<String> l = new ArrayList<String>();
	    
	    synchronized (this) {
	    	for (String name : entryPoints.keySet()) {
	            if (name.endsWith(".lslp")) { //$NON-NLS-1$
	                l.add(name);
	            }
	    	}
	    }
	    
	    String[] scripts = l.toArray(new String[l.size()]);
	    Arrays.sort(scripts);
	    return scripts;
	}
	
    public List<String> getLslModules() {
        ArrayList<String> l = new ArrayList<String>();
        if (entryPoints == null) return l;
        
        synchronized (this) {
	    	for (String name : entryPoints.keySet()) {
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
	
	private void hasError(IResource resource, Map<String,LinkedList<ErrInfo>> summary) {
		if (resource instanceof IFile) {
			IFile f = (IFile) resource;
			if (f.exists()) {
				try {
					
					resource.deleteMarkers(LSLPLUS_PROBLEM, true, IResource.DEPTH_ONE);
					String key = resource.getProjectRelativePath().toString();
					
					LinkedList<ErrInfo> status = summary.get(key);
					if (status != null && status.size() > 0) {
						for (ErrInfo ei0 : status) {
							ErrInfo_ErrInfo err = (ErrInfo_ErrInfo) ei0;
                            IMarker i = resource.createMarker(LSLPLUS_PROBLEM);
                            i.setAttribute(IMarker.MESSAGE, err.el2);
                            i.setAttribute(IMarker.SEVERITY,
                                    IMarker.SEVERITY_ERROR);
                            if (err.el1 instanceof Maybe_Just) {
                            	TextLocation_TextLocation errLoc = (TextLocation_TextLocation)
                            		((Maybe_Just<TextLocation>)err.el1).el1;
                                int lineOffset0 = errLoc.textLine0 - 1;
                                int lineOffset1 = errLoc.textLine1 - 1;
                                i.setAttribute(IMarker.LINE_NUMBER,
                                        errLoc.textLine0);
                                int[] offsets = Util.findOffsetsFor(new int[] {
                                        lineOffset0, lineOffset1 }, new int[] {
                                        errLoc.textColumn0 - 1,
                                        errLoc.textColumn1 - 1 }, f);
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
					    
                        Util.log("Marked " + key); //$NON-NLS-1$
					}
				} catch (CoreException e) {
					Util.error(e, "error reading file"); //$NON-NLS-1$
				}
			}
		}
	}
	
	public void resourceChanged(IResourceChangeEvent event) {
	    Util.log("resource changed in " + this.project.getName()); //$NON-NLS-1$
		final IResourceDelta delta = event.getDelta();

		//DeltaVisitor dv = new DeltaVisitor();
		BetterDeltaVisitor dv = new BetterDeltaVisitor(this.project);
		boolean recompileAll = false;
		try {
		    delta.accept(dv);
		    recompileAll = dv.isRecompileAll();
		} catch (CoreException e) {
			recompileAll = true;
			Util.error(e, e.getLocalizedMessage());
		}
		
		if (recompileAll) onRecompAll();
        scheduleBuild(recompileAll, dv.getAddsAndUpdates(), dv.getRemovals());
		
		final List<IResource> newDerivedResources = dv.getNewDerivedResources();
		if (newDerivedResources != null && newDerivedResources.size() > 0) {
			WorkspaceJob job = new WorkspaceJob("MarkDerived") { //$NON-NLS-1$

				public IStatus runInWorkspace(IProgressMonitor monitor)
						throws CoreException {
				    for (IResource r : newDerivedResources) {
						r.setDerived(true);
						if (r instanceof IFile) {
						    ((IFile)r).setCharset("UTF-8", monitor); //$NON-NLS-1$
						}
					}
					
					return new Status(IStatus.OK, LSLPLUS, Messages.ProjectNature_MARK_DERIVED_COMPLETE);
				}
				
			};
			
			job.schedule();
		}
	}

    public void scheduleBuild(final boolean recompileAll, final List<LslPlusElement> scriptChanges,
    		final List<LslPlusElement> scriptRemovals) {
        Util.log("check errors!"); //$NON-NLS-1$
        WorkspaceJob job = new WorkspaceJob("EvaluateErrors") { //$NON-NLS-1$

        	public IStatus runInWorkspace(IProgressMonitor monitor) {
        		checkForErrors(recompileAll, scriptChanges, scriptRemovals);
        		return new Status(IStatus.OK,LSLPLUS, Messages.ProjectNature_OK);
        	}
        	
        };
        
        job.schedule();
    }
	
	public void setProject(IProject project) {
		this.project = project;
		checkForErrors(true, null, null);
	}

	/**
	 * Gets the XML source list descriptor for the project.
	 * @return the LSL source list XML descriptor for the project
	 * @throws CoreException 
	 */
	public String projectSourceList() throws CoreException {
        final SourceListBuilder builder = new SourceListBuilder(false);
        IProject[] ps = project.getReferencedProjects();
        
        for (IProject p : ps) {
        	p.accept(builder);
        }
        builder.setModulesOnly(false);
        project.accept(builder);
        return builder.xmlDescriptor();
	}

    public synchronized LinkedList<GlobalSummary> getGlobalVariables(String fileName) {
    	globalVariables.get(fileName);
        return globalVariables.get(fileName);
    }
    
    public static String resourceToLslPlusName(IResource r) {
        return r.getProjectRelativePath().toString().replace('/', '.');
    }
    
    public void addRecompileListener(RecompileListener l) {
    	this.recompListeners.add(l);
    }
    
    public void removeRecompileListener(RecompileListener l) {
    	this.recompListeners.remove(l);
    }
    
    private void onRecompAll() {
    	for (RecompileListener l : recompListeners) {
    		l.recompile();
    	}
    }
}
