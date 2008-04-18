package lslplus.editor;

import java.io.ByteArrayInputStream;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.sim.SimProject;
import lslplus.sim.SimWorldDef;
import lslplus.sim.SimProject.Node;
import lslplus.util.Util;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.commands.operations.IUndoContext;
import org.eclipse.core.commands.operations.UndoContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.EditorPart;

public class SimEditor extends EditorPart implements SimProject.NodeListener {
    
    private class UpdateValueOperation extends AbstractOperation {
        private SimProject.Node n;
        private String value;
        private String oldValue;
        public UpdateValueOperation(SimProject.Node n, String value) {
            super("Update Value");
            this.n = n;
            this.value = value;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldValue = n.getValueString();
            n.updateValue(value);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldValue = n.getValueString();
            n.updateValue(value);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.updateValue(oldValue);
            decChangeCount();
            return Status.OK_STATUS;
        }
        
    }

    private class UpdateNameOperation extends AbstractOperation {
        private SimProject.Node n;
        private String name;
        private String oldName;
        public UpdateNameOperation(SimProject.Node n, String name) {
            super("Update Value");
            this.n = n;
            this.name = name;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldName = n.getNameDisplay();
            n.updateName(name);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldName = n.getNameDisplay();
            n.updateName(name);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.updateName(oldName);
            decChangeCount();
            return Status.OK_STATUS;
        }
        
    }

	private class AddNodeOperation extends AbstractOperation {
	    private SimProject.Node n;
	    private SimProject.NodeFactory factory;
	    private SimProject.Node addedNode = null;
        public AddNodeOperation(SimProject.Node n, SimProject.NodeFactory factory) {
            super("Add Child");
            this.n = n;
            this.factory = factory;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.addChild(addedNode = factory.createNode(n));
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.addChild(addedNode = factory.createNode(n));
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.removeChild(addedNode);
            decChangeCount();
            return Status.OK_STATUS;
        }
	    
	}
	
    private class AddAfterOperation extends AbstractOperation {
        private SimProject.Node n;
        private SimProject.NodeFactory factory;
        private SimProject.Node addedNode = null;
        public AddAfterOperation(SimProject.Node n, SimProject.NodeFactory factory) {
            super("Add after");
            this.n = n;
            this.factory = factory;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            SimProject.Node parent = n.getParent();
            if (parent != null) {
                parent.insertChildAfter(addedNode = factory.createNode(parent), n);
                incChangeCount();
            }
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            SimProject.Node parent = n.getParent();
            if (parent != null) {
                parent.insertChildAfter(addedNode = factory.createNode(parent), n);
                incChangeCount();
            }
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            if (n.getParent() != null) {
                n.getParent().removeChild(addedNode);
                decChangeCount();
            }
            return Status.OK_STATUS;
        }
        
    }
    
    private class AddBeforeOperation extends AbstractOperation {
        private SimProject.Node n;
        private SimProject.NodeFactory factory;
        private SimProject.Node addedNode = null;
        public AddBeforeOperation(SimProject.Node n, SimProject.NodeFactory factory) {
            super("Add before");
            this.n = n;
            this.factory = factory;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            SimProject.Node parent = n.getParent();
            if (parent != null) {
                incChangeCount();
                parent.insertChildBefore(addedNode = factory.createNode(parent), n);
            }
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            SimProject.Node parent = n.getParent();
            if (parent != null) {
                incChangeCount();
                parent.insertChildBefore(addedNode = factory.createNode(parent), n);
            }
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            if (n.getParent() != null) {
                n.getParent().removeChild(addedNode);
                decChangeCount();
            }
            return Status.OK_STATUS;
        }
    }
    
	private class DeleteNodeOperation extends AbstractOperation {
	    SimProject.Node parent;
	    SimProject.Node child;
        public DeleteNodeOperation(SimProject.Node parent, SimProject.Node child) {
            super("Delete node");
            this.parent = parent;
            this.child = child;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            parent.removeChild(child);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            parent.removeChild(child);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            parent.addChild(child);
            decChangeCount();
            return Status.OK_STATUS;
        }
	}
	
    private class AddNodeAction extends Action {
        private SimProject.NodeFactory factory;
        public AddNodeAction(SimProject.NodeFactory factory) {
            super("Add " + factory.getNodeTypeName()); //$NON-NLS-1$
            this.factory = factory;
        }
        
        public void run() {
            SimProject.Node n = getSelectedNode();
            AddNodeOperation operation = new AddNodeOperation(n, factory);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }
    
    private class AddNodeBeforeAction extends Action {
        private SimProject.NodeFactory factory;
        public AddNodeBeforeAction(SimProject.NodeFactory factory) {
            super("Add " + factory.getNodeTypeName()); //$NON-NLS-1$
            this.factory = factory;
        }
        
        public void run() {
            SimProject.Node n = getSelectedNode();
            AddBeforeOperation operation = new AddBeforeOperation(n, factory);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }
    
    private class AddNodeAfterAction extends Action {
        private SimProject.NodeFactory factory;
        public AddNodeAfterAction(SimProject.NodeFactory factory) {
            super("Add " + factory.getNodeTypeName()); //$NON-NLS-1$
            this.factory = factory;
        }
        
        public void run() {
            SimProject.Node n = getSelectedNode();
            AddAfterOperation operation = new AddAfterOperation(n, factory);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }
    
    private class DeleteNodeAction extends Action {
        public DeleteNodeAction() {
            super("Delete Node"); //$NON-NLS-1$
        }
        
        public void run() {
            SimProject.Node n = getSelectedNode();
            DeleteNodeOperation operation = new DeleteNodeOperation(n.getParent(), n);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }
    
    
	private class SimValueEditingSupport extends EditingSupport {
	    private CellEditor curEditor = null;
	    private String[] curChoices = null;
		private SimValueEditingSupport(ColumnViewer viewer, Shell shell) {
			super(viewer);
		}

		protected boolean canEdit(Object element) {
		    SimProject.Node n = (SimProject.Node) element;
			return n.isValueChangeable();
		}

		protected CellEditor getCellEditor(Object element) {
			final SimProject.Node n = (SimProject.Node) element;
			
			if (n.hasValueChoices()) {
			    String[] choices;
			    if ("scripts".equals(n.getChoicesId())) { //$NON-NLS-1$
			        choices = nature.getLslScripts();
			    } else if ("avatars".equals(n.getChoicesId())) {
			        SimProject.Node root = n.findRoot();
			        final LinkedList avnames = new LinkedList();
			        root.accept(new SimProject.NodeVisitor() {
                        public void visit(Node n) {
                            if (n instanceof SimProject.AvatarNode) {
                                avnames.add(n.getNameDisplay());
                            }
                        }
			        });
			        choices = (String[]) avnames.toArray(new String[avnames.size()]);
			    } else {
			        choices = new String[0];
			    }
			    curChoices = choices;
			    curEditor = new ComboBoxCellEditor(fTree, choices);
			} else {
			    curEditor = new TextCellEditor(fTree);
			    curEditor.setValidator(new ICellEditorValidator() {
                    public String isValid(Object value) {
                        SimProject.Status status = n.checkValueString(value.toString());
                        if (status.isOk()) return null;
                        return status.toString();
                    }
			        
			    });
			    
			}
			return curEditor;
		}

		protected Object getValue(Object element) {
			SimProject.Node n = (SimProject.Node) element;
			if (n.hasValueChoices()) {
			    String value = n.getValueString();
			    int index = -1;
			    for (int i = 0; i < curChoices.length; i++) {
			        if (curChoices[i].equals(value)) {
			            index = i;
			            break;
			        }
			    }
			    
			    return new Integer(index);
			} else {
			    return n.getValueString();
			}
		}

		protected void setValue(Object element, Object value) {
			SimProject.Node n = (SimProject.Node) element;
			String newValue = null;
			if (n.hasValueChoices()) {
			    Integer i = (Integer) value;
			    
			    if (i.intValue() < 0) return;
			    newValue = curChoices[i.intValue()];
			    //n.setValueFromString(curChoices[i.intValue()]);
			    
			} else {
			    if (value == null) return;
			    SimProject.Status status = n.checkValueString((String)value);
			    if (status.isOk()) newValue = (String)value;
			}
			
			if (newValue.equals(n.getValueString())) return;
			
            UpdateValueOperation operation = new UpdateValueOperation(n, newValue);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.log(e, e.getLocalizedMessage());
            }
		}
	}

   private class SimNameEditingSupport extends EditingSupport {
        private CellEditor curEditor = null;
        private SimNameEditingSupport(ColumnViewer viewer, Shell shell) {
            super(viewer);
        }

        protected boolean canEdit(Object element) {
            SimProject.Node n = (SimProject.Node) element;
            return n.isNameChangeable();
        }

        protected CellEditor getCellEditor(Object element) {
            final SimProject.Node n = (SimProject.Node) element;
            
            curEditor = new TextCellEditor(fTree);
            curEditor.setValidator(new ICellEditorValidator() {
                public String isValid(Object value) {
                    SimProject.Status status = n.checkNameString(value.toString());
                    if (status.isOk()) return null;
                    return status.toString();
                }
                
            });
            return curEditor;
        }

        protected Object getValue(Object element) {
            SimProject.Node n = (SimProject.Node) element;
            return n.getNameDisplay();
        }

        protected void setValue(Object element, Object value) {
            SimProject.Node n = (SimProject.Node) element;
            
            SimProject.Status status = n.checkNameString((String)value);
            if (!status.isOk()) return;
            if (value.equals(n.getNameDisplay())) return;
            UpdateNameOperation operation = new UpdateNameOperation(n, (String)value);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }

	private TreeColumn fColumn1;
	private TreeColumn fColumn2;
	private TreeViewer fTreeViewer;
	private Tree fTree;
	private SimProjectLabelProvider fLabelProvider;
	private SimProject.WorldNode world = null;
	private String simProjectName = null;
	private LslProjectNature nature = null;
	private int changeCount = 0;
	private IFile file;
	private DeleteNodeAction fDeleteNodeAction;
	private static final String BLANK = ""; //$NON-NLS-1$
	private IOperationHistory operationsHistory;
	private IUndoContext undoContext;
	
	public SimEditor() {
	}

	void incChangeCount() {
	    int curCount = changeCount;
	    changeCount++;
	    fireIfDirtyStateChanged(curCount);
    }

	void decChangeCount() {
        int curCount = changeCount;
        changeCount--;
        fireIfDirtyStateChanged(curCount);
    }

    private void fireIfDirtyStateChanged(int curCount) {
        if (curCount == 0 && changeCount != 0 ||
            changeCount == 0 && curCount != 0) firePropertyChange(PROP_DIRTY);
    }

	void zeroChangeCount() {
	    int curCount = changeCount;
	    changeCount = 0;
	    fireIfDirtyStateChanged(curCount);
	}
	
    public void doSave(IProgressMonitor monitor) {
		String val = SimProject.toXml(world);
		if (LslPlusPlugin.DEBUG) Util.log("world = " + val); //$NON-NLS-1$
		try {
			file.setContents(new ByteArrayInputStream(val.getBytes()), IResource.FORCE | IResource.KEEP_HISTORY, monitor);
			zeroChangeCount();
		} catch (CoreException e) {
			Util.log(e, e.getLocalizedMessage());
		}
		
		try {
		    SimWorldDef def = SimProject.toSimWorldDef(world);
		    Util.log(SimWorldDef.toXML(def));
		} catch (Exception e) {
		    Util.log(e, e.getLocalizedMessage());
		}
	}

	public void doSaveAs() {
	}

	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		this.setSite(site);  
		this.setInput(input);
        operationsHistory = this.getSite().getWorkbenchWindow().getWorkbench().getOperationSupport().getOperationHistory();
		undoContext = new UndoContext();
		this.setPartName(input.getName());
		file = (IFile) input.getAdapter(IFile.class);
		try {
			nature = (LslProjectNature) file.getProject().getNature(LslProjectNature.ID);
		} catch (CoreException e1) {
			throw new PartInitException("Can't get project nature", e1); //$NON-NLS-1$
		}
		IPath fullPath = file.getFullPath();
		simProjectName = fullPath.removeFileExtension().lastSegment();
		if (file != null) {
			try {
				world = SimProject.fromXml(file.getContents(), file);
			} catch (CoreException e) {
				Util.log(e, "Corrupted sim project file: " + e.getMessage()); //$NON-NLS-1$
				world = null;
			}
		}
	}

	public boolean isDirty() {
		return changeCount != 0;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public void createPartControl(Composite parent) {
		//control = new Composite(parent, SWT.NULL);
		//control.setVisible(true);
		createViewer(parent);
		createUndoRedoActions();
	}

	public void setFocus() {
	}

	private void createViewer(Composite parent) {
		fTreeViewer = new TreeViewer(parent, SWT.SINGLE | SWT.FULL_SELECTION);
		fTree = fTreeViewer.getTree();
		fTree.setLinesVisible(true);
		createColumns(fTree);
		world.addListener(this);
		fTreeViewer.setContentProvider(new SimProjectContentProvider(world));
		fTreeViewer.setLabelProvider(fLabelProvider = new SimProjectLabelProvider());
		fTreeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent e) {
				TreeSelection selection = (TreeSelection) e.getSelection();
				
			}
		});
		fTreeViewer.setInput(this);
		
		TreeViewerColumn column2 = new TreeViewerColumn(fTreeViewer, fColumn2);
		column2.setLabelProvider(new CellLabelProvider() {
			public void update(ViewerCell cell) {
			    SimProject.Node n = (SimProject.Node) cell.getElement();
			    cell.setImage(fLabelProvider.getColumnImage(n,1));
                if (n == null) cell.setText(BLANK);
				else cell.setText(n.getValueString());
			}
		});
        TreeViewerColumn column1 = new TreeViewerColumn(fTreeViewer, fColumn1);
        column1.setLabelProvider(new CellLabelProvider() {
            public void update(ViewerCell cell) {
                SimProject.Node n = (SimProject.Node) cell.getElement();
                cell.setImage(fLabelProvider.getColumnImage(n,0));
                if (n == null) cell.setText(BLANK);
                else cell.setText(n.getNameDisplay());
            }
        });
		
		column2.setEditingSupport(new SimValueEditingSupport(fTreeViewer, getShell()));
		column1.setEditingSupport(new SimNameEditingSupport(fTreeViewer, getShell()));
		
		fDeleteNodeAction = new DeleteNodeAction();
		
		MenuManager popupMenuManager = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		IMenuListener listener = new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				SimProject.Node n = getSelectedNode();
				SimProject.NodeFactory[] factories = n.legalChildNodes();
				
				if (factories.length > 0) {
				    MenuManager addChildSubMenu = new MenuManager("Add Child");
	                for (int i = 0; i < factories.length; i++) {
	                    manager.add(new AddNodeAction(factories[i]));
	                }
	                manager.add(addChildSubMenu);
				}
				
				SimProject.Node parent = n.getParent();
				if (parent != null) {
				    factories = parent.legalChildNodes();
				    if (factories.length > 0) {
    				    MenuManager addItemBeforeSubMenu = new MenuManager("Add Item Before");
                        MenuManager addItemAfterSubMenu = new MenuManager("Add Item After");
                        
                        for (int i = 0; i < factories.length; i++) {
                            addItemBeforeSubMenu.add(new AddNodeBeforeAction(factories[i]));
                            addItemAfterSubMenu.add(new AddNodeAfterAction(factories[i]));
                        }
                        manager.add(addItemBeforeSubMenu);
                        manager.add(addItemAfterSubMenu);
                        
				    }
				}
				if (n != null && n.isDeletable()) manager.add(fDeleteNodeAction);
			}
		};
		popupMenuManager.addMenuListener(listener);
		popupMenuManager.setRemoveAllWhenShown(true);
		//getSite().registerContextMenu(popupMenuManager, getSite().getSelectionProvider());
		Menu menu = popupMenuManager.createContextMenu(fTree);
		fTree.setMenu(menu);

	}

	private SimProject.Node getSelectedNode() {
		ITreeSelection selection = (ITreeSelection) fTreeViewer.getSelection();
		return (selection!=null) ? (SimProject.Node) selection.getFirstElement() : null;
		
	}
	
     
	private void createColumns(Tree tree) {
		fColumn1 = new TreeColumn(tree, SWT.LEFT);
		fColumn1.setText(Messages.getString("LslTestEditor.ITEM"));  //$NON-NLS-1$
		fColumn1.setWidth(200);
		fColumn1.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		fColumn2 = new TreeColumn(tree, SWT.LEFT);
		fColumn2.setText(Messages.getString("LslTestEditor.VALUE"));  //$NON-NLS-1$
		fColumn2.setWidth(100);
		fColumn2.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		tree.setHeaderVisible(true);
	}

	public void fillContextMenu(IMenuManager manager) {
	}

	private Shell getShell() {
        return SimEditor.this.getEditorSite().getShell();
    }

    public void nodeStructureChanged(final SimProject.Node n) {
        asyncExec(new Runnable() {  public void run() {
                fTreeViewer.refresh(n, true);
        }});
    }

    public void nodeValueChanged(final SimProject.Node n) {
        asyncExec(new Runnable() {  public void run() {
                fTreeViewer.update(n, null);
        }});
    }
    
    private void asyncExec(Runnable r) {
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(r);
    }
    
    protected void createUndoRedoActions() {
        UndoRedoActionGroup group = new UndoRedoActionGroup(getEditorSite(), undoContext, true);
        group.fillActionBars(getEditorSite().getActionBars());
    }
    
}
